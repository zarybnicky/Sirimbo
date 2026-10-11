import { TYPES } from '@dataplan/pg';
import { addPgTableCondition } from 'postgraphile/utils';

const plugins: GraphileConfig.Plugin[] = [
  ...(['event', 'person', 'competitor'] as const).map((tableName) =>
    addPgTableCondition(
      { schemaName: 'federated', tableName },
      'search',
      (build) => ({ type: build.graphql.GraphQLString }),
      (value, { sql, sqlTableAlias: row, sqlValueWithCodec }) => {
        if (typeof value !== 'string' || !value.trim()) return null;
        const search = sqlValueWithCodec(value.trim(), TYPES.text);
        if (tableName === 'event') {
          return sql`strpos(lower(concat_ws(' ', ${row}.name, ${row}.location, ${row}.city, ${row}.external_id)), lower(${search})) > 0`;
        }
        const name =
          tableName === 'person'
            ? sql`${row}.search_name`
            : sql`app_private.normalize_name(${row}.name)`;
        return sql`(${row}.external_id::text = ${search} or ${row}.id = ${search}
          or strpos(${name}, app_private.normalize_name(${search})) > 0)`;
      },
    ),
  ),
  addPgTableCondition(
    { schemaName: 'federated', tableName: 'competitor' },
    'competitionId',
    (build) => ({ type: build.getInputTypeByName('BigInt') }),
    (value, { sql, sqlTableAlias: row, sqlValueWithCodec }) => {
      if (value == null) return null;
      const competitionId = sqlValueWithCodec(value, TYPES.bigint);
      return sql`${row}.id in (
        select competitor_id from federated.competition_entry where competition_id = ${competitionId}
        union all
        select competitor_id from federated.competition_result where competition_id = ${competitionId}
      )`;
    },
  ),
  addPgTableCondition(
    { schemaName: 'federated', tableName: 'competition' },
    'competitorId',
    (build) => ({ type: build.graphql.GraphQLString }),
    (value, { sql, sqlTableAlias: row, sqlValueWithCodec }) => {
      if (value == null) return null;
      const competitorId = sqlValueWithCodec(value, TYPES.text);
      return sql`${row}.id in (
        select competition_id from federated.competition_entry where competitor_id = ${competitorId}
        union all
        select competition_id from federated.competition_result where competitor_id = ${competitorId}
      )`;
    },
  ),
  addPgTableCondition(
    { schemaName: 'federated', tableName: 'event' },
    'onDate',
    (build) => ({ type: build.getInputTypeByName('Date') }),
    (value, { sql, sqlTableAlias: row, sqlValueWithCodec }) =>
      value == null ? null : sql`${row}.range @> ${sqlValueWithCodec(value, TYPES.date)}`,
  ),
  addPgTableCondition(
    { schemaName: 'crawler', tableName: 'frontier' },
    'keyContains',
    (build) => ({ type: build.graphql.GraphQLString }),
    (value, { sql, sqlTableAlias: row, sqlValueWithCodec }) =>
      value == null || value === ''
        ? null
        : sql`strpos(${row}.key, ${sqlValueWithCodec(value, TYPES.text)}) > 0`,
  ),
];

export default plugins;
