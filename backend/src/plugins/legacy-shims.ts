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
