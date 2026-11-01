import assert from 'node:assert';
import { sql, TYPES, type PgSelectSingleStep, type PgSelectStep } from 'postgraphile/@dataplan/pg';
import { access, applyInput, constant, lambda, type ConnectionStep, type GrafastFieldConfig } from 'postgraphile/grafast';
import { extendSchema } from 'postgraphile/utils';

export const LegacyShimsPlugin = extendSchema((build) => ({
  typeDefs: `
    enum EventInstanceRangeScope {
      ALL
      MINE
      RELEVANT
    }

    extend type Query {
      eventInstancesForRangeList(
        onlyType: EventType
        startRange: Datetime
        endRange: Datetime
        trainerIds: [BigInt]
        participantIds: [BigInt]
        parentId: BigInt
        scope: EventInstanceRangeScope = ALL
        locationIds: [BigInt]
        onlyTypes: [EventType]
        first: Int
        offset: Int
      ): [Event!] @deprecated(reason: "Use eventsList with filters.")
    }

    type EventExternalRegistration {
      id: BigInt!
      note: String
      email: String!
      firstName: String!
      lastName: String!
    }

    extend type Event {
      remainingPersonSpots: Int
        @deprecated(reason: "Use registrationInfo.remainingCapacity.")
      eventExternalRegistrationsByInstanceIdList: [EventExternalRegistration!]!
        @deprecated(reason: "Use registrationsList.")
    }

    extend type EventTrainer {
      name: String @deprecated(reason: "Use person { name }.")
    }
  `,
  objects: {
    Query: {
      plans: {
        eventInstancesForRangeList($root, args, info) {
          const query = build.getTypeByName('Query');
          assert(build.graphql.isObjectType(query));
          const plan = query.getFields().eventsList.extensions?.grafast?.plan;
          assert(plan);
          const $events = plan($root, args, info) as PgSelectStep;
          const $condition = lambda(args.getRaw(), (input) => ({
            type: input.onlyType ?? undefined,
            types: input.onlyTypes,
            trainerIds: input.trainerIds,
            participant: input.participantIds == null ? undefined : { personIds: input.participantIds },
            locationIds: input.locationIds,
            parentId: input.parentId ?? undefined,
            onlyMine: input.scope === 'MINE' ||
              (input.scope === null && input.parentId != null),
            relevantOnly: input.scope === 'RELEVANT' && input.parentId == null,
          }));
          // Grafast accepts computed input steps; its input type excludes LambdaStep.
          $events.apply(applyInput(
            build.getInputTypeByName('EventCondition'),
            $condition as unknown as Parameters<typeof applyInput>[1],
            (qb) => qb.whereBuilder(),
          ));

          // Preserve the old function's tenant restriction and range semantics,
          // including omitted/null bounds and zero-length or reversed windows.
          const row = $events.alias;
          const start = $events.placeholder(args.$startRange, TYPES.timestamptz);
          const end = $events.placeholder(args.$endRange, TYPES.timestamptz);
          const scope = $events.placeholder(args.$scope, TYPES.text);
          const parent = $events.placeholder(args.$parentId, TYPES.bigint);
          $events.where(sql`${row}.tenant_id = public.current_tenant_id()`);
          $events.where(sql`(${scope} is not null or ${parent} is not null)`);
          $events.where(sql`${start} is not null
            and ${row}.since < coalesce(${end}, 'infinity'::timestamptz)
            and app_private.tstzrange_overlaps(${row}.range, tstzrange(
              ${start}, greatest(${start}, coalesce(${end}, 'infinity'::timestamptz)), '[]'))`);
          $events.orderBy({ attribute: 'since', direction: 'ASC' });
          $events.setFirst(args.$first);
          $events.setOffset(args.$offset);
          return $events;
        },
      },
    },
    EventTrainer: {
      plans: {
        name($trainer, args, info) {
          const trainer = build.getTypeByName('EventTrainer');
          assert(build.graphql.isObjectType(trainer));
          const plan = trainer.getFields().person.extensions?.grafast?.plan;
          assert(plan);
          return (plan($trainer, args, info) as PgSelectSingleStep).get('name');
        },
      },
    },
    Event: {
      plans: {
        remainingPersonSpots($event, args, info) {
          const event = build.getTypeByName('Event');
          assert(build.graphql.isObjectType(event));
          const plan = event.getFields().registrationInfo.extensions?.grafast?.plan;
          assert(plan);
          return access(plan($event, args, info), 'remaining_capacity');
        },
        eventExternalRegistrationsByInstanceIdList: () => constant([]),
      },
    },
  },
}));

const legacyFields: Record<string, Record<string, string>> = {
  Tenant: { tenantLocationsList: 'locationsList' },
  Query: { eventInstance: 'event' },
  Mutation: {
    updateEventInstance: 'updateEvent',
    deleteEventInstance: 'deleteEvent',
  },
  Event: {
    childEventInstancesList: 'childEventsList',
    eventInstanceTrainersByInstanceIdList: 'eventTrainersList',
    eventInstanceRegistrationsByInstanceIdList: 'registrationsList',
  },
  EventRegistration: {
    instanceId: 'eventId',
    eventLessonDemandsByRegistrationIdList: 'lessonRequestsList',
  },
  Person: {
    eventInstanceRegistrationsList: 'eventRegistrationsList',
    allCouplesList: 'couplesList',
  },
  SaveEventsPayload: { eventInstances: 'result' },
  UpsertAnnouncementPayload: { announcement: 'result' },
  UpdateEventPayload: { eventInstance: 'event' },
  DeleteEventPayload: { eventInstance: 'event' },
};

export const LegacyRelationAliasesPlugin: GraphileConfig.Plugin = {
  name: 'LegacyRelationAliasesPlugin',
  schema: {
    hooks: {
      GraphQLObjectType_fields: {
        after: ['PgRelationsPlugin', 'PgMutationUpdateDeletePlugin', 'PgCustomTypeFieldPlugin', 'PersonCouplesPlugin'],
        callback(fields, build, { Self }) {
          const { GraphQLInputObjectType, GraphQLNonNull, getNamedType, isInputObjectType } =
            build.graphql;
          for (const [legacy, canonical] of Object.entries(legacyFields[Self.name] ?? {})) {
            if (!fields[canonical]) continue;
            const field = fields[canonical];
            let args = field.args;
            if (Self.name === 'Mutation') {
              const input = getNamedType(args!.input.type);
              assert(isInputObjectType(input));
              // Keep variable declarations using the old input name, including input-field plans.
              args = {
                ...args,
                input: {
                  ...args!.input,
                  type: new GraphQLNonNull(
                    new GraphQLInputObjectType({
                      ...input.toConfig(),
                      name: `${build.inflection.upperCamelCase(legacy)}Input`,
                    }),
                  ),
                },
              };
            }
            fields = {
              ...fields,
              [legacy]: {
                ...field,
                args,
                deprecationReason: `Use ${canonical}.`,
              },
            };
          }
          if (Self.name === 'Person') {
            const couples = fields.couplesList;
            const couplesPlan = couples.extensions?.grafast?.plan;
            if (!couplesPlan) throw new Error('Expected person couplesList plan.');
            fields = build.extend(fields, {
              activeCouplesList: {
                ...couples,
                deprecationReason: 'Use couplesList with condition.status: ACTIVE.',
                extensions: {
                  ...couples.extensions,
                  grafast: {
                    ...couples.extensions?.grafast,
                    plan($person, args, info) {
                      const $couples = couplesPlan($person, args, info) as PgSelectStep;
                      $couples.where(sql`${$couples.alias}.status = 'active'`);
                      return $couples;
                    },
                  },
                },
              },
            }, 'Legacy person partnerships.');
            const query = build.getTypeByName('Query');
            if (!build.graphql.isObjectType(query)) throw new Error('Expected Query object type.');
            const canonical = query.toConfig().fields.competitions;
            const plan = canonical.extensions?.grafast?.plan;
            if (!plan) throw new Error('Expected competitions plan.');

            const field: GrafastFieldConfig<PgSelectSingleStep> = {
              ...canonical,
              type: new build.graphql.GraphQLNonNull(build.graphql.getNullableType(canonical.type)),
              description: 'Competitions entered by this person, across federations.',
              deprecationReason: 'Use competitions with condition.competitor.personIds.',
              args: {
                ...canonical.args,
                first: {
                  ...canonical.args!.first,
                  defaultValue: 30,
                  // The field plan applies first so last can override its default.
                  applyPlan: () => undefined,
                },
              },
              extensions: { ...canonical.extensions, boundedPagination: true },
              plan($person, args, info) {
                const $connection = plan($person, args, info) as ConnectionStep<unknown>;
                $connection.setFirst(lambda([args.$first, args.$last], ([first, last]) =>
                  last == null ? first ?? 30 : null));
                const $competitions = $connection.getSubplan() as PgSelectStep;
                const personId = $competitions.placeholder($person.get('id'));
                $competitions.where(sql`${$competitions.alias}.id = any(array(
                  select participation.competition_id
                  from federated.competition_participation participation
                  where participation.competitor_id = any(array(
                    select component.competitor_id
                    from public.person_federation_link link
                    join federated.person person
                      on (person.federation, person.external_id) = (link.federation, link.external_id)
                    join federated.competitor_component component on component.person_id = person.id
                    where link.person_id = ${personId}
                  ))
                ))`);
                return $connection;
              },
            };
            fields = build.extend(fields, { federationCompetitions: field }, 'Legacy person competitions.');
          }
          return fields;
        },
      },
    },
  },
};
