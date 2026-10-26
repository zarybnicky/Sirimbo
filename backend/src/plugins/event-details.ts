import { pgClassExpression, type PgSelectSingleStep } from 'postgraphile/@dataplan/pg';
import { wrapPlans } from 'postgraphile/utils';

export const EventDetailsPlugin = wrapPlans((build) => {
  // Treat the composite as one value. Its children read the decoded record,
  // so PostgreSQL calculates it once, after sorting and limiting the events.
  const registrationInfoCodec = {
    ...build.input.pgRegistry.pgResources.event_registration_info.codec,
    attributes: undefined,
  };
  return {
    Event: {
      registrationInfo(_plan, $source) {
        const $event = $source as PgSelectSingleStep;
        return pgClassExpression($event, registrationInfoCodec)`public.event_registration_info(${$event.record()})`;
      },
    },
  };
}, { name: 'EventDetailsPlugin' });

EventDetailsPlugin.schema = {
  ...EventDetailsPlugin.schema,
  hooks: {
    ...EventDetailsPlugin.schema?.hooks,
    GraphQLObjectType(spec) {
      return spec.name === 'EventRegistrationInfo'
        ? { ...spec, assertStep: undefined }
        : spec;
    },
  },
};

