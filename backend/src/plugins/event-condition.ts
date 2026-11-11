import { LIST_TYPES, TYPES } from 'postgraphile/@dataplan/pg';
import { applyInput, lambda, object } from 'postgraphile/grafast';
import { GraphQLError } from 'postgraphile/graphql';
import { addPgTableCondition, extendSchema } from 'postgraphile/utils';

type EventRangeCondition = { start: string; end?: string | null };

const savedSearchConditions: Record<string, string> = {
  'Query.events': 'EventCondition',
  'Query.eventsList': 'EventCondition',
  'Query.competitions': 'CompetitionCondition',
  'Query.federationEvents': 'FederationEventCondition',
  'Competition.entriesList': 'CompetitionEntryCondition',
  'Competition.resultsList': 'CompetitionResultCondition',
  'Competition.officials': 'CompetitionOfficialCondition',
  'FederationEvent.officials': 'FederationEventOfficialCondition',
};

const plugins: GraphileConfig.Plugin[] = [
  {
    name: 'EventSavedSearchPlugin',
    schema: {
      hooks: {
        GraphQLObjectType_fields_field_args(args, build, { Self, scope }) {
          const conditionType = savedSearchConditions[`${Self.name}.${scope.fieldName}`];
          if (!conditionType) return args;
          return {
            ...args,
            savedSearch: {
              type: build.getInputTypeByName('BigInt'),
              description: 'Apply one of your saved searches within the current tenant.',
              applyPlan(_, $field, arg) {
                const $id = arg.getRaw();
                // Resolve the saved search before planning the nested list filters.
                const $condition = $id.withLayerPlan(() => {
                  const $search = build.input.pgRegistry.pgResources.event_saved_search.get({ id: $id });
                  return lambda(object({ id: $id, filter: $search.get('condition') }), ({ id, filter }) => {
                    if (id == null) return {};
                    if (filter == null) throw new GraphQLError('Saved search not found.');
                    if (conditionType === 'EventCondition') return {
                      types: filter.types,
                      trainerIds: filter.trainer_ids,
                      locationIds: filter.location_ids,
                      parentId: filter.parent_id ?? undefined,
                      participant: filter.participant_ids == null && filter.cohort_id == null ? undefined : {
                        personIds: filter.participant_ids,
                        cohortId: filter.cohort_id,
                      },
                      onlyMine: filter.only_mine,
                      relevantOnly: filter.relevant_only,
                      canRegister: filter.can_register,
                    };
                    const participant = {
                      personIds: filter.with_competitions ? filter.participant_ids : [],
                      cohortId: filter.cohort_id,
                      onlyMine: filter.only_mine,
                    };
                    const official = {
                      ids: participant.personIds,
                      onlyMine: filter.only_mine,
                      membership: {
                        state: filter.participant_ids == null && !filter.only_mine ? 'current' : undefined,
                        inCohorts: filter.cohort_id == null ? undefined : [filter.cohort_id],
                      },
                    };
                    if (conditionType === 'CompetitionCondition') return { involvement: { competitor: participant, official } };
                    if (conditionType === 'FederationEventCondition') return { official };
                    if (conditionType.endsWith('OfficialCondition')) return { person: official };
                    return { competitor: participant };
                  });
                });
                const $select = scope.isPgFieldConnection ? $field.getSubplan() : $field;
                $select.apply(applyInput(build.getInputTypeByName(conditionType),
                  $condition as unknown as Parameters<typeof applyInput>[1],
                  (qb) => qb.whereBuilder()));
              },
            },
          };
        },
      },
    },
  },
  extendSchema({
    typeDefs: `
      input EventRangeCondition {
        """Inclusive start of the time window."""
        start: Datetime!
        """Exclusive end of the time window. Omit for an unbounded end."""
        end: Datetime
      }
    `,
  }),
  addPgTableCondition(
    { schemaName: 'public', tableName: 'event_registration' },
    'excludeCohortSource',
    (build) => ({
      type: build.graphql.GraphQLBoolean,
      description: 'Exclude automatic cohort registrations; retain explicit and legacy registrations.',
    }),
    (value, { sql, sqlTableAlias: row }) =>
      value ? sql`${row}.source is distinct from 'cohort'::public.event_registration_source` : null,
  ),
  addPgTableCondition(
    { schemaName: 'public', tableName: 'event' },
    'canRegister',
    (build) => ({
      type: build.graphql.GraphQLBoolean,
      description: 'Whether a self-service registration is available.',
    }),
    (value, { sql, sqlTableAlias: row, sqlValueWithCodec }) =>
      value == null
        ? null
        : sql`(public.event_registration_info(${row})).can_register = ${sqlValueWithCodec(value, TYPES.boolean)}`,
  ),
  addPgTableCondition(
    { schemaName: 'public', tableName: 'event' },
    'overlaps',
    (build) => ({ type: build.getInputTypeByName('EventRangeCondition') }),
    (value, { sql, sqlTableAlias: row, sqlValueWithCodec }) => {
      if (value == null) return null;
      const { start, end } = value as EventRangeCondition;
      const since = sqlValueWithCodec(start, TYPES.timestamptz);
      const until = sqlValueWithCodec(end ?? null, TYPES.timestamptz);
      return sql`app_private.tstzrange_overlaps(${row}.range, tstzrange(${since}, ${until}, '[)'))`;
    },
  ),
  ...(['onlyMine', 'relevantOnly'] as const).map((field) =>
    addPgTableCondition(
      { schemaName: 'public', tableName: 'event' },
      field,
      (build) => ({
        type: build.graphql.GraphQLBoolean,
        description: field === 'onlyMine'
          ? 'Only events where the viewer has an active registration or trainer assignment.'
          : 'Only top-level events, the viewer’s events, and children of the viewer’s events.',
      }),
      (value, { sql, sqlTableAlias: row }) => {
        if (!value) return null;
        const mine = sql`
          select event_id from public.event_registration
          where person_id = any((select public.current_person_ids())::bigint[])
            and registration_status = 'active'
          union all
          select event_id from public.event_trainer
          where person_id = any((select public.current_person_ids())::bigint[])
        `;
        return field === 'onlyMine'
          ? sql`${row}.id in (${mine})`
          : sql`(${row}.parent_id is null
              or ${row}.id in (${mine}) or ${row}.parent_id in (${mine}))`;
      },
    ),
  ),
  ...(['types', 'trainerIds', 'locationIds'] as const).map((field) =>
    addPgTableCondition(
      { schemaName: 'public', tableName: 'event' },
      field,
      (build) => {
        const type = build.getInputTypeByName(field === 'types' ? 'EventType' : 'BigInt');
        return {
          type: new build.graphql.GraphQLList(
            field === 'types' ? new build.graphql.GraphQLNonNull(type) : type,
          ),
        };
      },
      (value, { sql, sqlTableAlias: row, sqlValueWithCodec }) => {
        const values = value as (string | null)[] | null;
        if (values == null) return null;
        if (values.length === 0) return sql`false`;
        if (field === 'types')
          return sql`${row}.type = any(${sqlValueWithCodec(values, LIST_TYPES.text)}::public.event_type[])`;
        const ids = sqlValueWithCodec(values, LIST_TYPES.bigint);
        if (field === 'locationIds') return sql`${row}.location_id = any(${ids})`;
        return sql`exists (
          select 1 from public.event_trainer p
          where p.event_id = ${row}.id and p.person_id = any(${ids})
        )`;
      },
    ),
  ),
];

export default plugins;
