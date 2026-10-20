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

export const LegacyRelationAliasesPlugin: GraphileConfig.Plugin = {
  name: 'LegacyRelationAliasesPlugin',
  schema: {
    hooks: {
      GraphQLObjectType_fields: {
        after: ['PgRelationsPlugin'],
        callback(fields, _build, { Self }) {
          if (Self.name !== 'EventInstance' || !fields.eventTrainersList) return fields;
          return {
            ...fields,
            eventInstanceTrainersByInstanceIdList: {
              ...fields.eventTrainersList,
              deprecationReason: 'Use eventTrainersList.',
            },
          };
        },
      },
    },
  },
};
