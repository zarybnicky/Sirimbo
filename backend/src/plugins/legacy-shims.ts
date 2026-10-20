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

    extend type EventInstance {
      eventExternalRegistrationsByInstanceIdList: [EventExternalRegistration!]!
        @deprecated(reason: "Use eventInstanceRegistrationsByInstanceIdList.")
    }
  `,
  objects: {
    EventInstance: {
      plans: {
        eventExternalRegistrationsByInstanceIdList: () => constant([]),
      },
    },
  },
});

const legacyFields: Record<string, Record<string, string>> = {
  Tenant: { tenantLocationsList: 'locationsList' },
  EventInstance: { eventInstanceTrainersByInstanceIdList: 'eventTrainersList' },
  EventInstanceRegistration: { eventLessonDemandsByRegistrationIdList: 'lessonRequestsList' },
  SetLessonDemandPayload: { eventLessonDemand: 'eventLessonRequest' },
};

export const LegacyRelationAliasesPlugin: GraphileConfig.Plugin = {
  name: 'LegacyRelationAliasesPlugin',
  schema: {
    hooks: {
      GraphQLObjectType_fields: {
        after: ['PgRelationsPlugin'],
        callback(fields, _build, { Self }) {
          for (const [legacy, canonical] of Object.entries(legacyFields[Self.name] ?? {})) {
            if (!fields[canonical]) continue;
            fields = {
              ...fields,
              [legacy]: {
                ...fields[canonical],
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
