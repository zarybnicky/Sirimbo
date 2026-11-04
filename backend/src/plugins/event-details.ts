import { pgClassExpression, PgSelectStep, sql, TYPES, type PgSelectSingleStep } from 'postgraphile/@dataplan/pg';
import type { ConnectionStep } from 'postgraphile/grafast';
import { extendSchema, wrapPlans } from 'postgraphile/utils';

export const EventTrainerFieldsPlugin = extendSchema((build) => ({
  typeDefs: `extend type EventTrainer { tenantTrainer: TenantTrainer }`,
  objects: {
    EventTrainer: {
      plans: {
        tenantTrainer($trainer: PgSelectSingleStep) {
          const resources = build.input.pgRegistry.pgResources;
          const $event = resources.event.get({
            id: $trainer.get('event_id'),
            tenant_id: $trainer.get('tenant_id'),
          });
          const $relationship = resources.tenant_trainer.find({
            tenant_id: $event.get('tenant_id'),
            person_id: $trainer.get('person_id'),
          });
          const since = $relationship.placeholder($event.get('since'), TYPES.timestamptz);
          $relationship.where(sql`${$relationship.alias}.active_range @> ${since}`);
          return $relationship.single();
        },
      },
    },
  },
}), 'EventTrainerFieldsPlugin');

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

