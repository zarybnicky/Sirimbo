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
    { schemaName: 'federated', tableName: 'competition_entry' },
    'hasResult',
    (build) => ({ type: build.graphql.GraphQLBoolean }),
    (value, { sql, sqlTableAlias: row }) => {
      if (value == null) return null;
      const exists = sql`exists (
        select 1 from federated.competition_result r
        where r.competition_id = ${row}.competition_id and r.competitor_id = ${row}.competitor_id
      )`;
      return value ? exists : sql`not ${exists}`;
    },
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
  ...(['eventId', 'competitionId'] as const).map((field) =>
    addPgTableCondition(
      { schemaName: 'crawler', tableName: 'frontier' },
      field,
      (build) => ({ type: build.getInputTypeByName('BigInt') }),
      (value, { sql, sqlTableAlias: row, sqlValueWithCodec }) => {
        if (value == null) return null;
        const id = sqlValueWithCodec(value, TYPES.bigint);
        const eventId =
          field === 'eventId'
            ? id
            : sql`(select event_id from federated.competition where id = ${id})`;
        const competitionScope =
          field === 'eventId' ? sql`c.event_id = ${id}` : sql`c.id = ${id}`;
        return sql`(${row}.federation, ${row}.kind, ${row}.key) in (
          select e.federation, k.kind, e.external_id
          from federated.event e
          cross join unnest(array['event', 'eventCompetitors']) k(kind)
          where e.id = ${eventId} and e.federation = 'csts'
          union all
          select c.federation, k.kind, c.external_id
          from federated.competition c
          cross join lateral unnest(case c.federation
            when 'csts' then array['competitionResults']
            when 'wdsf' then array['competition', 'participantIndex', 'officialIndex']
            else array[]::text[]
          end) k(kind)
          where ${competitionScope}
        )`;
      },
    ),
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
