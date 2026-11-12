import { pgClassExpression, LIST_TYPES, sql, sqlValueWithCodec, TYPES, type PgSelectSingleStep, type PgSelectStep } from 'postgraphile/@dataplan/pg';
import type { GrafastFieldConfig } from 'postgraphile/grafast';
import { addPgTableCondition, extendSchema } from 'postgraphile/utils';
import personMembershipPlugins, { membershipCandidateIds } from './person-membership-condition.ts';

const PersonFieldsPlugin = extendSchema((build) => ({
  typeDefs: `
    extend type Person {
      cohortIds: [BigInt]
      isMember: Boolean
      isTrainer: Boolean
      isAdmin: Boolean
      isExternal: Boolean!
    }
  `,
  objects: {
    Person: {
      plans: {
        cohortIds($person: PgSelectSingleStep) {
          const $cohorts = build.input.pgRegistry.pgResources.cohort_membership.find({
            person_id: $person.get('id'),
          }).clone('aggregate');
          $cohorts.where(sql`now() <@ ${$cohorts.alias}.active_range`);
          $cohorts.setInliningForbidden();
          return pgClassExpression($cohorts.single(), LIST_TYPES.bigint)`array_agg(${$cohorts.alias}.cohort_id)`;
        },
        isMember($person: PgSelectSingleStep) {
          return pgClassExpression($person, TYPES.boolean)`
            ${$person.get('id')} in (select person_id from public.current_tenant_membership)`;
        },
        isTrainer($person: PgSelectSingleStep) {
          return pgClassExpression($person, TYPES.boolean)`
            ${$person.get('id')} in (select person_id from public.current_tenant_trainer)`;
        },
        isAdmin($person: PgSelectSingleStep) {
          return pgClassExpression($person, TYPES.boolean)`
            ${$person.get('id')} in (select person_id from public.current_tenant_administrator)`;
        },
        isExternal($person: PgSelectSingleStep) {
          return pgClassExpression($person, TYPES.boolean)`
            ${$person.get('id')} not in (
              select person_id from public.current_tenant_membership
              union all
              select person_id from public.current_tenant_trainer
              union all
              select person_id from public.current_tenant_administrator
            )`;
        },
      },
    },
  },
}), 'PersonFieldsPlugin');

const TenantNamesPlugin = extendSchema((build) => {
  const tenant = build.input.pgRegistry.pgResources.tenant;
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
    'ids',
    (build) => ({
      type: new build.graphql.GraphQLList(
        new build.graphql.GraphQLNonNull(build.getInputTypeByName('BigInt')),
      ),
    }),
    (value, { sqlTableAlias: row }) =>
      value == null
        ? null
        : sql`${row}.id = any(${sqlValueWithCodec(value, LIST_TYPES.bigint)})`,
  ),
  addPgTableCondition(
    { schemaName: 'public', tableName: 'person' },
    'onlyMine',
    (build) => ({ type: build.graphql.GraphQLBoolean }),
    (value, { sqlTableAlias: row }) =>
      value ? sql`${row}.id = any((select public.current_person_ids())::bigint[])` : null,
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
      const candidates = membershipCandidateIds(value);
      return candidates ? sql`exists (
        select 1 from (${candidates}) membership
        where membership.person_id in (${row}.man_id, ${row}.woman_id)
      )` : null;
    },
  ),
  {
    name: 'PersonCouplesPlugin',
    schema: {
      hooks: {
        GraphQLObjectType_fields: {
          after: ['PgRelationsPlugin'],
          callback(fields, build, { Self }) {
            if (Self.name !== 'Person') return fields;
            const query = build.getTypeByName('Query');
            if (!build.graphql.isObjectType(query)) throw new Error('Expected Query object type.');
            const canonical = query.toConfig().fields.couplesList;
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
                    $couples.where(sql`(${$couples.alias}.man_id = ${personId} or ${$couples.alias}.woman_id = ${personId})`);
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
