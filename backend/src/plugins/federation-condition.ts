import { TYPES } from '@dataplan/pg';
import { addPgTableCondition, extendSchema } from 'postgraphile/utils';

type CategoryProgressCondition = {
  ageGroups?: string[];
  classes?: string[];
  disciplines?: string[];
};

const plugins: GraphileConfig.Plugin[] = [
  extendSchema({
    typeDefs: `
      input CompetitorCategoryProgressCondition {
        ageGroups: [String!]
        classes: [String!]
        disciplines: [String!]
      }
    `,
  }),
  addPgTableCondition(
    { schemaName: 'federated', tableName: 'competitor' },
    'categoryProgress',
    (build) => ({
      type: build.getInputTypeByName('CompetitorCategoryProgressCondition'),
    }),
    (value, { sql, sqlTableAlias: row, sqlValueWithCodec }) => {
      if (value == null) return null;
      const { ageGroups, classes, disciplines } = value as CategoryProgressCondition;
      const predicates = (
        [
          ['age_group_code', ageGroups],
          ['class', classes],
          ['discipline', disciplines],
        ] as const
      ).flatMap(([column, values]) =>
        values?.length
          ? [
              sql`cat.${sql.identifier(column)} in (${sql.join(
                values.map((v) =>
                  column === 'age_group_code'
                    ? sql`federated.resolve_age_group(${sqlValueWithCodec(v, TYPES.text)})`
                    : sqlValueWithCodec(v, TYPES.text),
                ),
                ', ',
              )})`,
            ]
          : [],
      );
      if (!predicates.length) return null;
      return sql`exists (
          select 1 from federated.competitor_category_progress p
          join federated.category cat on cat.id = p.category_id
          where p.competitor_id = ${row}.id and ${sql.join(predicates, ' and ')}
        )`;
    },
  ),
  addPgTableCondition(
    { schemaName: 'federated', tableName: 'competitor' },
    'competitorTypes',
    (build) => ({
      type: new build.graphql.GraphQLList(
        new build.graphql.GraphQLNonNull(build.getInputTypeByName('CompetitorType')),
      ),
    }),
    (value, { sql, sqlTableAlias: row, sqlValueWithCodec }) => {
      const values = value as string[] | null;
      return values?.length
        ? sql`${row}.competitor_type::text in (${sql.join(
            values.map((v) => sqlValueWithCodec(v, TYPES.text)),
            ', ',
          )})`
        : null;
    },
  ),
  addPgTableCondition(
    { schemaName: 'federated', tableName: 'competitor' },
    'competedSince',
    (build) => ({ type: build.getInputTypeByName('Date') }),
    (value, { sql, sqlTableAlias: row, sqlValueWithCodec }) =>
      value == null
        ? null
        : sql`exists (
      select 1 from federated.competition_result r
      join federated.competition c on c.id = r.competition_id
      where r.competitor_id = ${row}.id
        and c.start_date >= ${sqlValueWithCodec(value, TYPES.date)} and c.start_date <= current_date
    )`,
  ),
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
  ...(['event', 'competition'] as const).map((tableName) =>
    addPgTableCondition(
      { schemaName: 'federated', tableName },
      'officialPersonId',
      (build) => ({ type: build.graphql.GraphQLString }),
      (value, { sql, sqlTableAlias: row, sqlValueWithCodec }) => {
        if (value == null) return null;
        const personId = sqlValueWithCodec(value, TYPES.text);
        return tableName === 'competition'
          ? sql`${row}.id in (
              select competition_id from federated.competition_official where person_id = ${personId}
            )`
          : sql`${row}.id in (
              select event_id from federated.event_official where person_id = ${personId}
              union all
              select c.event_id from federated.competition c
              join federated.competition_official o on o.competition_id = c.id
              where o.person_id = ${personId}
            )`;
      },
    ),
  ),
  addPgTableCondition(
    { schemaName: 'federated', tableName: 'ranklist_entry' },
    'latestSnapshot',
    (build) => ({ type: build.graphql.GraphQLBoolean }),
    (value, { sql, sqlTableAlias: row }) => {
      if (value == null) return null;
      const latest = sql`${row}.snapshot_id in (
        select distinct on (ranklist_id, kind) id
        from federated.ranklist_snapshot
        order by ranklist_id, kind, as_of_date desc
      )`;
      return value ? latest : sql`not (${latest})`;
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
