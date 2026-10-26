import { sql, TYPES, type PgSelectSingleStep, type PgSelectStep } from 'postgraphile/@dataplan/pg';
import type { GrafastFieldConfig } from 'postgraphile/grafast';
import { addPgTableCondition } from 'postgraphile/utils';
import { membershipCandidateIds } from './person-membership-condition.ts';

const plugins: GraphileConfig.Plugin[] = [
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
                    $couples.where(sql`(${$couples.alias}.man_id = ${personId}
                      or ${$couples.alias}.woman_id = ${personId})`);
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
