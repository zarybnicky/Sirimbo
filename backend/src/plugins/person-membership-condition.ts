import { LIST_TYPES, TYPES } from '@dataplan/pg';
import { addPgTableCondition } from 'postgraphile/utils';
import type { GraphQLInputType } from 'graphql';

type SqlTag = GraphileBuild.Build['sql'];
type SQL = ReturnType<SqlTag>;

type MembershipState = 'current' | 'former';

type MembershipCondition = {
  state?: MembershipState | null;
  states?: readonly MembershipState[] | null;
  inCohorts?: readonly string[] | null;
  isTrainer?: boolean | null;
  isAdmin?: boolean | null;
};

function expectInputType(build: GraphileBuild.Build, name: string): GraphQLInputType {
  const type = build.getTypeByName(name);
  if (!type || !build.graphql.isInputType(type)) {
    throw new Error(`Failed to resolve ${name} input type`);
  }
  return type;
}

function isMembershipCondition(value: unknown): value is MembershipCondition {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function expectMembershipCondition(value: unknown): MembershipCondition | null {
  if (value == null) return null;
  if (!isMembershipCondition(value)) {
    throw new Error('Invalid person membership condition');
  }
  return value;
}

function expectState(value: unknown): MembershipState {
  if (value == null) return 'current';
  if (value === 'current' || value === 'former') return value;
  throw new Error(`Invalid person membership state: ${String(value)}`);
}

function expectCohorts(value: unknown): readonly string[] | null {
  if (value == null) return null;
  if (!Array.isArray(value) || !value.every((item) => typeof item === 'string')) {
    throw new Error('Invalid person membership cohort filter');
  }
  return value;
}

const PersonMembershipConditionTypesPlugin: GraphileConfig.Plugin = {
  name: 'PersonMembershipConditionTypesPlugin',
  version: '0.0.0',
  schema: {
    hooks: {
      init(_, build) {
        const {
          graphql: { GraphQLBoolean, GraphQLList, GraphQLNonNull },
        } = build;

        build.registerEnumType(
          'PersonMembershipState',
          {},
          () => ({
            description: 'Which tenant membership state a person membership filter targets.',
            values: {
              CURRENT: { value: 'current' },
              FORMER: { value: 'former' },
            },
          }),
          'PersonMembershipConditionTypesPlugin',
        );

        build.registerInputObjectType(
          'PersonMembershipCondition',
          {},
          () => ({
            description: 'Filters people by current or former tenant membership state.',
            fields: () => {
              const stateType = expectInputType(build, 'PersonMembershipState');
              const bigintType = expectInputType(build, 'BigInt');

              return {
                state: {
                  description: 'The membership state to filter by.',
                  type: stateType,
                },
                states: {
                  description: 'Membership states to include. An empty list includes current and former people; overrides state.',
                  type: new GraphQLList(new GraphQLNonNull(stateType)),
                },
                inCohorts: {
                  description:
                    'Cohort IDs to match. An empty list means people without a matching cohort membership.',
                  type: new GraphQLList(new GraphQLNonNull(bigintType)),
                },
                isTrainer: {
                  description: 'Whether the person has a trainer relationship in the selected state.',
                  type: GraphQLBoolean,
                },
                isAdmin: {
                  description:
                    'Whether the person has an administrator relationship in the selected state.',
                  type: GraphQLBoolean,
                },
              };
            },
          }),
          'PersonMembershipConditionTypesPlugin',
        );

        return _;
      },
    },
  },
};

const PersonMembershipConditionPlugin = addPgTableCondition(
  { schemaName: 'public', tableName: 'person' },
  'membership',
  (build) => {
    return {
      description:
        'Filters people by tenant membership, cohort membership, trainer status, and administrator status.',
      type: expectInputType(build, 'PersonMembershipCondition'),
    };
  },
  (rawValue, { sql, sqlTableAlias: personAlias, sqlValueWithCodec }) => {
    const condition = expectMembershipCondition(rawValue);
    if (condition == null) return null;

    const states = condition.states == null
      ? [expectState(condition.state)]
      : condition.states.length ? condition.states.map(expectState) : ['current', 'former'];
    const cohorts = expectCohorts(condition.inCohorts);
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
    return predicates.length ? sql`${personAlias}.id in (
      select m.person_id
      from unnest(${sqlValueWithCodec(states, LIST_TYPES.text)}) s(state)
      cross join lateral app_private.person_membership_candidates(s.state) m
      where ${sql.join(predicates, ' and ')}
    )` : null;
  },
);

const plugins: GraphileConfig.Plugin[] = [
  PersonMembershipConditionTypesPlugin,
  PersonMembershipConditionPlugin,
];

export default plugins;
