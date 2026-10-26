import assert from 'node:assert';
import { sql, type PgSelectSingleStep, type PgSelectStep } from 'postgraphile/@dataplan/pg';
import { access, constant, lambda, type ConnectionStep, type GrafastFieldConfig } from 'postgraphile/grafast';
import { extendSchema } from 'postgraphile/utils';

export const LegacyShimsPlugin = extendSchema((build) => ({
  typeDefs: `
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
              deprecationReason: 'Use competitions with condition.participant.personIds.',
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
