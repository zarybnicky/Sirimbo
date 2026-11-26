import { LIST_TYPES, TYPES } from 'postgraphile/@dataplan/pg';
import {
  applyInput, bakedInput, inhibitOnNull, lambda, object,
  trap, TRAP_INHIBITED, type Step,
} from 'postgraphile/grafast';
import { GraphQLError, GraphQLList, type GraphQLEnumType } from 'postgraphile/graphql';
import { addPgTableCondition, extendSchema, wrapPlans } from 'postgraphile/utils';

type EventRangeCondition = { start: string; end?: string | null };

const calendarConditions: Record<string, string> = {
  'Query.events': 'EventCondition',
  'Query.eventsList': 'EventCondition',
  'Query.federationEvents': 'FederationEventCondition',
  'Query.people': 'PersonCondition',
  'FederationEvent.participationsList': 'CompetitionParticipationCondition',
  'FederationEvent.officialAssignmentsList': 'OfficialAssignmentCondition',
  'Event.registrationsList': 'EventRegistrationCondition',
  'Query.eventFacetsList': 'EventSearchConditionInput',
  'Query.eventConflictsList': 'EventSearchConditionInput',
};

function calendarFilter(
  build: Pick<GraphileBuild.Build, 'input'>,
  $id: Step,
  $input: Step,
) {
  return $id.withLayerPlan(() => {
    const $search = build.input.pgRegistry.pgResources.event_saved_search!.get({ id: inhibitOnNull($id) });
    return lambda(
      object({ id: $id, filter: trap($search.get('condition'), TRAP_INHIBITED), input: $input }),
      ({ id, filter, input }) => {
        if (id == null) return input;
        if (filter == null) throw new GraphQLError('Saved search not found.');
        return {
          types: filter.types,
          trainerIds: filter.trainer_ids,
          locationIds: filter.location_ids,
          parentId: filter.parent_id,
          participantIds: filter.participant_ids,
          coupleId: filter.couple_id,
          cohortId: filter.cohort_id,
          onlyMine: filter.only_mine,
          relevantOnly: filter.relevant_only,
          canRegister: filter.can_register,
          withCompetitions: filter.with_competitions ?? false,
        };
      },
    );
  });
}

const savedSearchPlugin = wrapPlans(
  ({ Self, scope }, build) => {
    const conditionType = calendarConditions[`${Self.name}.${scope.fieldName}`];
    return conditionType ? { conditionType, build, fieldName: scope.fieldName, isConnection: scope.isPgFieldConnection } : null;
  },
  ({ conditionType, build, fieldName, isConnection }) => (plan, _source, args) => {
    const $filter = calendarFilter(build, args.getRaw('savedSearch'), args.getRaw('calendarFilter'));
    const $native = conditionType === 'EventSearchConditionInput'
      ? bakedInput(args.typeAt('condition'), args.getRaw('condition')) : args.getRaw('condition');
    // applyInput is planned at the root, including for nested collection fields.
    const $condition = $filter.withLayerPlan(() => lambda(
      object({ filter: $filter, native: $native }),
      ({ filter, native }) => {
        if (conditionType === 'EventSearchConditionInput') {
          if (filter == null) return native;
          return {
            types: filter.types,
            trainer_ids: filter.trainerIds,
            location_ids: filter.locationIds,
            parent_id: filter.parentId,
            participant_ids: filter.participantIds,
            couple_id: filter.coupleId,
            cohort_id: filter.cohortId,
            only_mine: filter.onlyMine,
            relevant_only: filter.relevantOnly,
            can_register: filter.canRegister,
          };
        }
        if (filter == null) return [native];

        const hasPeopleSelection = filter.participantIds != null || filter.coupleId != null
          || filter.cohortId != null;
        const participant = {
          personIds: filter.participantIds,
          coupleId: filter.coupleId,
          cohortId: filter.cohortId,
          onlyMine: hasPeopleSelection ? false : filter.onlyMine,
        };
        if (['FederationEventCondition', 'CompetitionParticipationCondition', 'OfficialAssignmentCondition'].includes(conditionType)
          && (filter.withCompetitions === false || filter.canRegister)) participant.personIds = [];
        const person = {
          id: participant.personIds == null ? undefined : { eqIn: participant.personIds },
          coupleId: participant.coupleId,
          onlyMine: participant.onlyMine,
          membership: {
            state: participant.personIds == null && participant.coupleId == null && !participant.onlyMine
              ? 'current' : undefined,
            inCohorts: participant.cohortId == null ? undefined : [participant.cohortId],
          },
        };
        let mapped;
        if (conditionType === 'EventCondition') {
          const eventTypes = (build.getInputTypeByName('EventType') as GraphQLEnumType)
            .getValues().map(({ value }) => value as string);
          mapped = {
            type: filter.canRegister
              ? { eqIn: (filter.types ?? eventTypes).filter((type: string) => type !== 'group') }
              : filter.types == null ? undefined : { eqIn: filter.types },
            trainers: filter.trainerIds == null ? undefined : { personId: { eqIn: filter.trainerIds } },
            locationId: filter.locationIds == null ? undefined : { eqIn: filter.locationIds },
            parentId: filter.parentId ?? undefined,
            participant: {
              personIds: filter.participantIds,
              coupleId: filter.coupleId,
              cohortId: filter.cohortId,
            },
            onlyMine: filter.onlyMine,
            relevantOnly: filter.relevantOnly,
            canRegister: filter.canRegister,
          };
        } else if (conditionType === 'PersonCondition') {
          mapped = person;
        } else if (conditionType === 'EventRegistrationCondition') {
          mapped = { participant };
        } else if (conditionType === 'FederationEventCondition') {
          mapped = { participant };
        } else if (conditionType === 'OfficialAssignmentCondition') {
          mapped = { person };
        } else if (conditionType === 'CompetitionParticipationCondition') {
          mapped = { competitor: participant };
        }
        return conditionType === 'FederationEventCondition' && filter.onlyMine && hasPeopleSelection
          ? [native, { participant: { onlyMine: true } }, mapped]
          : [native, mapped];
      },
    ));

    if (conditionType === 'EventSearchConditionInput') {
      const resource = build.input.pgRegistry.pgResources[fieldName === 'eventConflictsList' ? 'event_conflicts' : 'event_facets']!;
      return resource.execute([
        { step: args.getRaw('startRange'), pgCodec: TYPES.timestamptz },
        { step: args.getRaw('endRange'), pgCodec: TYPES.timestamptz },
        { step: $condition, pgCodec: resource.parameters![2]!.codec },
      ]);
    }
    const $field = plan();
    const $select = isConnection ? $field.getSubplan() : $field;
    // Native and saved conditions share the date bounds on one builder.
    $select.apply(applyInput(new GraphQLList(build.getInputTypeByName(conditionType)),
      $condition as unknown as Parameters<typeof applyInput>[1], (qb) => qb.whereBuilder()));
    return $field;
  },
  { name: 'EventSavedSearchPlugin' },
);

savedSearchPlugin.schema = {
  ...savedSearchPlugin.schema,
  hooks: {
    ...savedSearchPlugin.schema?.hooks,
    GraphQLObjectType_fields_field_args: {
      after: ['PgConditionArgumentPlugin'],
      callback(args, build, { Self, scope }) {
        if (!calendarConditions[`${Self.name}.${scope.fieldName}`]) return args;
        return {
          ...args,
          condition: { ...args.condition!, applyPlan() {} },
          calendarFilter: {
            type: build.getInputTypeByName('CalendarFilterInput'),
            description: 'Apply calendar selectors in addition to the native condition.',
          },
          savedSearch: {
            type: build.getInputTypeByName('BigInt'),
            description: 'Apply one of your saved searches within the current tenant, replacing calendarFilter.',
          },
        };
      },
    },
  },
};

const plugins: GraphileConfig.Plugin[] = [
  savedSearchPlugin,
  extendSchema({
    typeDefs: `
      """Calendar selectors. Collection switches and date bounds are supplied separately."""
      input CalendarFilterInput {
        types: [EventType!]
        trainerIds: [BigInt!]
        locationIds: [BigInt!]
        parentId: BigInt
        participantIds: [BigInt!]
        coupleId: BigInt
        cohortId: BigInt
        onlyMine: Boolean
        relevantOnly: Boolean
        canRegister: Boolean
      }

      input EventRangeCondition {
        """Inclusive start of the time window."""
        start: Datetime!
        """Exclusive end of the time window. Omit for an unbounded end."""
        end: Datetime
      }
    `,
  }),
  addPgTableCondition(
    { schemaName: 'public', tableName: 'event' },
    'types',
    (build) => ({
      type: new build.graphql.GraphQLList(
        new build.graphql.GraphQLNonNull(build.getInputTypeByName('EventType')),
      ),
      deprecationReason: 'Use type: { eqIn: [...] }.',
    }),
    (value, { sql, sqlTableAlias: row, sqlValueWithCodec }) => value == null
      ? null
      : sql`${row}.type = any(${sqlValueWithCodec(value as string[], LIST_TYPES.text)}::public.event_type[])`,
  ),
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
];

export default plugins;
