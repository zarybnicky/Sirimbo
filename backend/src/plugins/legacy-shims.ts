import { sql, type PgSelectSingleStep, type PgSelectStep } from 'postgraphile/@dataplan/pg';
import { constant, lambda, type ConnectionStep, type GrafastFieldConfig } from 'postgraphile/grafast';
import { extendSchema } from 'postgraphile/utils';

export const LegacyShimsPlugin = extendSchema({
  typeDefs: `
    type EventExternalRegistration {
      id: BigInt!
      note: String
      email: String!
      firstName: String!
      lastName: String!
    }

    extend type Event {
      eventExternalRegistrationsByInstanceIdList: [EventExternalRegistration!]!
        @deprecated(reason: "Use registrationsList.")
    }
  `,
  objects: {
    Event: {
      plans: {
        eventExternalRegistrationsByInstanceIdList: () => constant([]),
      },
    },
  },
});

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
  Person: { eventInstanceRegistrationsList: 'eventRegistrationsList' },
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
        after: ['PgRelationsPlugin', 'PgMutationUpdateDeletePlugin', 'PgCustomTypeFieldPlugin'],
        callback(fields, build, { Self }) {
          const { GraphQLInputObjectType, GraphQLNonNull, getNamedType, isInputObjectType } =
            build.graphql;
          for (const [legacy, canonical] of Object.entries(legacyFields[Self.name] ?? {})) {
            if (!fields[canonical]) continue;
            const field = fields[canonical];
            let args = field.args;
            if (Self.name === 'Mutation') {
              const input = getNamedType(args!.input.type);
              if (!isInputObjectType(input)) {
                throw new Error(`Expected an input object for ${canonical}.`);
              }
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
