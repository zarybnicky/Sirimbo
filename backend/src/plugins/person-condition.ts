import { LIST_TYPES, TYPES, sql, sqlValueWithCodec } from 'postgraphile/@dataplan/pg';
import { addPgTableCondition } from 'postgraphile/utils';

const plugins: GraphileConfig.Plugin[] = [
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
];

export default plugins;
