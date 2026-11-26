import { pgClassExpression, LIST_TYPES, sql, sqlValueWithCodec, TYPES, type PgSelectSingleStep, type PgSelectStep } from 'postgraphile/@dataplan/pg';
import type { GrafastFieldConfig } from 'postgraphile/grafast';
import { addPgTableCondition, extendSchema } from 'postgraphile/utils';
import personMembershipPlugins, { membershipCandidateIds, type MembershipCondition } from './person-membership-condition.ts';

const PersonFieldsPlugin = extendSchema(() => ({
  typeDefs: `
    extend type Person {
      cohortIds: [BigInt]
      tenantIds: [BigInt]
      """Current roles in the active tenant. Empty for external people."""
      roles: [TenantRole!]!
      isTrainer: Boolean @deprecated(reason: "Use roles containing TRAINER.")
      isAdmin: Boolean @deprecated(reason: "Use roles containing ADMINISTRATOR.")
    }
  `,
  objects: {
    Person: {
      plans: {
        cohortIds($person: PgSelectSingleStep) {
          return pgClassExpression($person, LIST_TYPES.bigint)`(
            select nullif(current_cohort_ids, '{}'::bigint[]) from app_private.tenant_relation
            where tenant_id = (select public.current_tenant_id()) and person_id = ${$person.get('id')}
          )`;
        },
        tenantIds($person: PgSelectSingleStep) {
          return pgClassExpression($person, LIST_TYPES.bigint)`array(
            select tenant_id from app_private.tenant_relation
            where person_id = ${$person.get('id')} and has_roles
          )`;
        },
        roles($person: PgSelectSingleStep) {
          return pgClassExpression($person, LIST_TYPES.text)`coalesce((
            select current_roles::text[] from app_private.current_tenant_relation
            where person_id = ${$person.get('id')}
          ), '{}'::text[])`;
        },
        isTrainer($person: PgSelectSingleStep) {
          return pgClassExpression($person, TYPES.boolean)`
            ${$person.get('id')} in (select person_id from app_private.current_tenant_relation where 'trainer' = any(current_roles))`;
        },
        isAdmin($person: PgSelectSingleStep) {
          return pgClassExpression($person, TYPES.boolean)`
            ${$person.get('id')} in (select person_id from app_private.current_tenant_relation where 'administrator' = any(current_roles))`;
        },
      },
    },
  },
}), 'PersonFieldsPlugin');

const TenantNamesPlugin = extendSchema((build) => {
  const tenant = build.input.pgRegistry.pgResources.tenant!;
  const plans = {
    tenantName($membership: PgSelectSingleStep) {
      const $tenant = tenant.get({ id: $membership.get('tenant_id') });
      $tenant.getClassStep().setInliningForbidden();
      return $tenant.get('name');
    },
  };
  return {
    typeDefs: `
      extend type TenantMembership { tenantName: String }
      extend type TenantTrainer { tenantName: String }
      extend type TenantAdministrator { tenantName: String }
    `,
    objects: {
      TenantMembership: { plans },
      TenantTrainer: { plans },
      TenantAdministrator: { plans },
    },
  };
}, 'TenantNamesPlugin');

const plugins: GraphileConfig.Plugin[] = [
  ...personMembershipPlugins,
  PersonFieldsPlugin,
  TenantNamesPlugin,
  addPgTableCondition(
    { schemaName: 'public', tableName: 'person' },
    'birthday',
    (build) => ({ type: build.getInputTypeByName('DateRangeCondition') }),
    (value, { sqlTableAlias: row }) => {
      if (value == null) return null;
      const { since, until } = value as { since?: string | null; until?: string | null };
      const birthday = sql`public.person_birthday(${row},
        coalesce(${sqlValueWithCodec(since ?? null, TYPES.date)}, current_date))`;
      return until == null
        ? sql`${birthday} is not null`
        : sql`${birthday} < ${sqlValueWithCodec(until, TYPES.date)}`;
    },
  ),
  addPgTableCondition(
    { schemaName: 'public', tableName: 'person' },
    'onlyMine',
    (build) => ({ type: build.graphql.GraphQLBoolean }),
    (value, { sqlTableAlias: row }) =>
      value ? sql`${row}.id = any((select public.current_person_ids())::bigint[])` : null,
  ),
  addPgTableCondition(
    { schemaName: 'public', tableName: 'person' },
    'coupleId',
    (build) => ({
      description: 'Match either partner of this couple.',
      type: build.getInputTypeByName('BigInt'),
    }),
    (value, { sqlTableAlias: row }) => {
      if (value == null) return null;
      const coupleId = sqlValueWithCodec(value, TYPES.bigint);
      return sql`${row}.id in (
        select man_id from public.couple where id = ${coupleId}
        union all
        select woman_id from public.couple where id = ${coupleId}
      )`;
    },
  ),
  addPgTableCondition(
    { schemaName: 'public', tableName: 'couple' },
    'personId',
    (build) => ({
      description: 'Match either partner of the couple.',
      type: build.getInputTypeByName('BigInt'),
    }),
    (value, { sqlTableAlias: row, sqlValueWithCodec }) => {
      if (value == null) return null;
      const personId = sqlValueWithCodec(value, TYPES.bigint);
      return sql`(${row}.man_id = ${personId} or ${row}.woman_id = ${personId})`;
    },
  ),
  addPgTableCondition(
    { schemaName: 'public', tableName: 'couple' },
    'membership',
    (build) => ({
      description: 'Require at least one partner to match the current-tenant person membership condition.',
      type: build.getInputTypeByName('PersonMembershipCondition'),
    }),
    (value, { sqlTableAlias: row }) => {
      const candidates = membershipCandidateIds(value as MembershipCondition | null);
      return candidates ? sql`exists (
        select 1 from (${candidates}) membership
        where membership.person_id in (${row}.man_id, ${row}.woman_id)
      )` : null;
    },
  ),
  {
    name: 'PeopleRelationsPlugin',
    schema: {
      hooks: {
        GraphQLObjectType_fields: {
          after: ['PgRelationsPlugin'],
          callback(fields, build, { Self }) {
            const batchedRelations = Self.name === 'CohortMembership' ? ['cohort']
              : Self.name === 'Couple' ? ['man', 'woman', 'image']
              : Self.name === 'Person' ? ['image'] : [];
            for (const name of batchedRelations) {
              const field = fields[name]!;
              const plan = field.plan!;
              fields = {
                ...fields,
                [name]: {
                  ...field,
                  plan($record, args, info) {
                    const $related = plan($record, args, info) as PgSelectSingleStep;
                    // Evaluate shared records and their RLS once per batch, outside nested rows.
                    $related.getClassStep().setInliningForbidden();
                    return $related;
                  },
                },
              };
            }
            if (Self.name !== 'Person') return fields;
            const query = build.getTypeByName('Query');
            if (!build.graphql.isObjectType(query)) throw new Error('Expected Query object type.');
            const canonical = query.toConfig().fields.couplesList!;
            const plan = canonical.extensions?.grafast?.plan;
            if (!plan) throw new Error('Expected couplesList plan.');

            const field: GrafastFieldConfig<PgSelectSingleStep> = {
              ...canonical,
              type: new build.graphql.GraphQLNonNull(build.graphql.getNullableType(canonical.type)),
              description: 'Partnerships involving this person.',
              extensions: {
                ...canonical.extensions,
                grafast: {
                  ...canonical.extensions?.grafast,
                  plan($person, args, info) {
                    const $couples = plan($person, args, info) as PgSelectStep;
                    const personId = $couples.placeholder($person.get('id'));
                    // Find partner IDs before ordering; otherwise RLS costs favor a full primary-key scan per person.
                    $couples.where(sql`${$couples.alias}.id = any(array(
                      select id from public.couple
                      where man_id = ${personId} or woman_id = ${personId}
                    ))`);
                    return $couples;
                  },
                },
              },
            };
            return build.extend(fields, { couplesList: field }, 'Person partnerships.');
          },
        },
      },
    },
  },
];

export default plugins;
