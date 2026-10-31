import { pgClassExpression, PgSelectStep, type PgSelectSingleStep } from 'postgraphile/@dataplan/pg';
import type { ConnectionStep } from 'postgraphile/grafast';
import { wrapPlans } from 'postgraphile/utils';

export const EventDetailsPlugin = wrapPlans((build) => {
  // Treat the composite as one value. Its children read the decoded record,
  // so PostgreSQL calculates it once, after sorting and limiting the events.
  const registrationInfoCodec = {
    ...build.input.pgRegistry.pgResources.event_registration_info.codec,
    attributes: undefined,
  };
  return {
    // Batch cohort lookups so their RLS checks are not repeated for each event.
    EventTargetCohort: {
      cohort(plan) {
        const $cohort = plan() as PgSelectSingleStep;
        $cohort.getClassStep().setInliningForbidden();
        return $cohort;
      },
    },
    EventRegistration: {
      couple(plan, $source) {
        const $registration = $source as PgSelectSingleStep;
        const $couple = plan() as PgSelectSingleStep;
        if ($registration.getClassStep().getMetaRaw('batchPreviewCouples')) {
          $couple.getClassStep().setInliningForbidden();
        }
        return $couple;
      },
    },
    Event: {
      registrations(plan, $source) {
        const $event = $source as PgSelectSingleStep;
        const $connection = plan() as ConnectionStep<unknown>;
        if (!PgSelectStep.getStaticInfo($event.getClassStep()).isUnique) {
          ($connection.getSubplan() as PgSelectStep).setMeta('batchPreviewCouples', true);
        }
        return $connection;
      },
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

