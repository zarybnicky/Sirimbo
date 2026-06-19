import { addPgTableCondition } from 'postgraphile/utils';
import type { GraphQLInputType } from 'graphql';

type SqlTag = GraphileBuild.Build['sql'];
type SQL = ReturnType<SqlTag>;

type MembershipState = 'current' | 'former';

type MembershipCondition = {
  state?: MembershipState | null;
  inCohorts?: readonly string[] | null;
  isTrainer?: boolean | null;
  isAdmin?: boolean | null;
};

type BirthDateRangeCondition = {
  since?: string | null;
  until?: string | null;
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

function isBirthDateRangeCondition(value: unknown): value is BirthDateRangeCondition {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function expectDateString(value: unknown, fieldName: string): string {
  if (typeof value !== 'string' || !/^\d{4}-\d{2}-\d{2}$/.test(value)) {
    throw new Error(`Invalid person birth date range ${fieldName}`);
  }
  return value;
}

function expectBirthDateRangeCondition(value: unknown): BirthDateRangeCondition | null {
  if (value == null) return null;
  if (!isBirthDateRangeCondition(value)) {
    throw new Error('Invalid person birth date range condition');
  }
  const since = expectDateString(value.since, 'since');
  const until = expectDateString(value.until, 'until');
  if (until <= since) {
    throw new Error('Person birth date range until must be after since');
  }
  return { since, until };
}

function visibleMembershipPredicate(state: MembershipState, sql: SqlTag, personAlias: SQL) {
  return state === 'current'
    ? sql`${personAlias}.id in (
        select tm.person_id from public.current_tenant_membership tm
        union
        select tt.person_id from public.current_tenant_trainer tt
        union
        select ta.person_id from public.current_tenant_administrator ta
      )`
    : sql`${personAlias}.id in (
        select tm.person_id
        from public.tenant_membership tm
        where tm.tenant_id = (select public.current_tenant_id())
          and tm.status = 'expired'
          and not exists (
            select 1
            from public.current_tenant_membership active
            where active.person_id = tm.person_id
          )
      )`;
}

function rolePredicate(
  roleTable: 'current_tenant_trainer' | 'current_tenant_administrator',
  historicalTable: 'tenant_trainer' | 'tenant_administrator',
  flag: boolean | null | undefined,
  state: MembershipState,
  sql: SqlTag,
  personAlias: SQL,
) {
  if (flag == null) return null;

  const exists =
    state === 'current'
      ? sql`exists (
          select 1
          from ${sql.identifier('public', roleTable)} role
          where role.person_id = ${personAlias}.id
        )`
      : sql`exists (
          select 1
          from ${sql.identifier('public', historicalTable)} role
          where role.person_id = ${personAlias}.id
            and role.tenant_id = (select public.current_tenant_id())
            and role.status = 'expired'
        )`;

  return flag ? exists : sql`not (${exists})`;
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
                  description: 'The membership state to filter by. Defaults to CURRENT.',
                  type: stateType,
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

        build.registerInputObjectType(
          'PersonBirthDateRangeCondition',
          {},
          () => ({
            description:
              'Filters people by annual birthday occurrence in a date range. The range is since-inclusive and until-exclusive.',
            fields: () => {
              const dateType = expectInputType(build, 'Date');

              return {
                since: {
                  description: 'Inclusive start date.',
                  type: new GraphQLNonNull(dateType),
                },
                until: {
                  description: 'Exclusive end date.',
                  type: new GraphQLNonNull(dateType),
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
  (rawValue, { sql, sqlTableAlias: personAlias, sqlValueWithCodec, build }) => {
    const condition = expectMembershipCondition(rawValue);
    if (condition == null) return null;

    const state = expectState(condition.state);
    const cohorts = expectCohorts(condition.inCohorts);
    const predicates: SQL[] = [visibleMembershipPredicate(state, sql, personAlias)];

    if (cohorts) {
      if (cohorts.length === 0) {
        predicates.push(
          state === 'current'
            ? sql`not exists (
                select 1
                from public.current_cohort_membership cm
                where cm.person_id = ${personAlias}.id
              )`
            : sql`not exists (
                select 1
                from public.cohort_membership cm
                where cm.person_id = ${personAlias}.id
                  and cm.tenant_id = (select public.current_tenant_id())
                  and cm.status = 'expired'
              )`,
        );
      } else {
        const cohortIdCodec =
          build.input.pgRegistry.pgResources.cohort?.codec.attributes.id?.codec;
        if (!cohortIdCodec) {
          throw new Error('Failed to resolve cohort ID codec');
        }
        const values = cohorts.map((cohort) => sqlValueWithCodec(cohort, cohortIdCodec));
        predicates.push(
          state === 'current'
            ? sql`exists (
                select 1
                from public.current_cohort_membership cm
                where cm.person_id = ${personAlias}.id
                  and cm.cohort_id in (${sql.join(values, ', ')})
              )`
            : sql`exists (
                select 1
                from public.cohort_membership cm
                where cm.person_id = ${personAlias}.id
                  and cm.tenant_id = (select public.current_tenant_id())
                  and cm.status = 'expired'
                  and cm.cohort_id in (${sql.join(values, ', ')})
              )`,
        );
      }
    }

    const trainerPredicate = rolePredicate(
      'current_tenant_trainer',
      'tenant_trainer',
      condition.isTrainer,
      state,
      sql,
      personAlias,
    );
    if (trainerPredicate) predicates.push(trainerPredicate);

    const adminPredicate = rolePredicate(
      'current_tenant_administrator',
      'tenant_administrator',
      condition.isAdmin,
      state,
      sql,
      personAlias,
    );
    if (adminPredicate) predicates.push(adminPredicate);

    return sql`(${sql.join(predicates, ') and (')})`;
  },
);

const PersonBirthDateRangeConditionPlugin = addPgTableCondition(
  { schemaName: 'public', tableName: 'person' },
  'birthDateInRange',
  (build) => {
    return {
      description:
        'Filters people whose next annual birthday occurrence falls inside the date range.',
      type: expectInputType(build, 'PersonBirthDateRangeCondition'),
    };
  },
  (rawValue, { sql, sqlTableAlias: personAlias, sqlValueWithCodec, build }) => {
    const range = expectBirthDateRangeCondition(rawValue);
    if (range == null) return null;

    const dateCodec =
      build.input.pgRegistry.pgResources.person?.codec.attributes.birth_date?.codec;
    if (!dateCodec) {
      throw new Error('Failed to resolve person birth_date codec');
    }

    const since = sqlValueWithCodec(range.since, dateCodec);
    const until = sqlValueWithCodec(range.until, dateCodec);
    const years = sql`(
      extract(year from ${since})::int - extract(year from ${personAlias}.birth_date)::int
    )`;
    const birthdayThisYear = sql`(
      ${personAlias}.birth_date + make_interval(years => ${years})
    )::date`;
    const birthdayNextYear = sql`(
      ${personAlias}.birth_date + make_interval(years => (${years} + 1))
    )::date`;
    const birthday = sql`(
      case
        when ${birthdayThisYear} >= ${since} then ${birthdayThisYear}
        else ${birthdayNextYear}
      end
    )`;

    return sql`(
      ${personAlias}.birth_date is not null
      and ${birthday} >= ${personAlias}.birth_date
      and ${birthday} >= ${since}
      and ${birthday} < ${until}
    )`;
  },
);

const plugins: GraphileConfig.Plugin[] = [
  PersonMembershipConditionTypesPlugin,
  PersonMembershipConditionPlugin,
  PersonBirthDateRangeConditionPlugin,
];

export default plugins;
