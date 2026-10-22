import { constant } from 'postgraphile/grafast';
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
          return fields;
        },
      },
    },
  },
};
