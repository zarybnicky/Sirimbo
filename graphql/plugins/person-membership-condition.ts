import { LIST_TYPES, TYPES, sql, sqlValueWithCodec } from 'postgraphile/@dataplan/pg';
import { addPgTableCondition, extendSchema } from 'postgraphile/utils';

type SqlTag = GraphileBuild.Build['sql'];
type SQL = ReturnType<SqlTag>;

type MembershipState = 'current' | 'former';

export type MembershipCondition = {
  state?: MembershipState | null;
  states?: readonly MembershipState[] | null;
  inCohorts?: readonly string[] | null;
  isTrainer?: boolean | null;
  isAdmin?: boolean | null;
};

export function membershipCandidateIds(condition: MembershipCondition | null | undefined): SQL | null {
  if (condition == null) return null;

  const states = condition.states == null
    ? [condition.state ?? 'current']
    : condition.states.length ? condition.states : ['current', 'former'];
  const cohorts = condition.inCohorts;
  const predicates: SQL[] = [];
  if (condition.states != null || condition.state != null) predicates.push(sql`m.in_state`);
  if (cohorts) {
    predicates.push(cohorts.length === 0
      ? sql`cardinality(m.cohort_ids) = 0`
      : sql`m.cohort_ids && ${sqlValueWithCodec(cohorts, LIST_TYPES.bigint)}`);
  }
  for (const [column, value] of [
    ['is_trainer', condition.isTrainer],
    ['is_admin', condition.isAdmin],
  ] as const) {
    if (value != null) predicates.push(sql`m.${sql.identifier(column)} = ${sqlValueWithCodec(value, TYPES.boolean)}`);
  }
  return predicates.length ? sql`
    select m.person_id
    from unnest(${sqlValueWithCodec(states, LIST_TYPES.text)}) s(state)
    cross join lateral app_private.person_membership_candidates(s.state) m
    where ${sql.join(predicates, ' and ')}
  ` : null;
}

const plugins: GraphileConfig.Plugin[] = [
  extendSchema({
    typeDefs: `
      """Which tenant membership state a person membership filter targets."""
      enum PersonMembershipState { CURRENT FORMER }
      """Filters people by current or former tenant membership state."""
      input PersonMembershipCondition {
        """The membership state to filter by."""
        state: PersonMembershipState
        """Membership states to include. An empty list includes current and former people; overrides state."""
        states: [PersonMembershipState!]
        """Cohort IDs to match. An empty list means people without a matching cohort membership."""
        inCohorts: [BigInt!]
        """Whether the person has a trainer relationship in the selected state."""
        isTrainer: Boolean
        """Whether the person has an administrator relationship in the selected state."""
        isAdmin: Boolean
      }
    `,
    enums: {
      PersonMembershipState: { values: { CURRENT: 'current', FORMER: 'former' } },
    },
  }),
  addPgTableCondition(
    { schemaName: 'public', tableName: 'person' },
    'membership',
    (build) => ({
      description: 'Filters people by tenant membership, cohort membership, trainer status, and administrator status.',
      type: build.getInputTypeByName('PersonMembershipCondition'),
    }),
    (value, { sqlTableAlias: personAlias }) => {
      const candidates = membershipCandidateIds(value as MembershipCondition | null);
      return candidates ? sql`${personAlias}.id in (${candidates})` : null;
    },
  ),
];

export default plugins;
