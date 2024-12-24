create or replace function app_private.register_new_cohort_member_to_events(c cohort_membership)
    returns setof event_registration
    language sql as $$
  insert into event_registration (event_id, target_cohort_id, person_id, tenant_id)
  select event.id, event_target_cohort.id, cohort_membership.person_id, cohort_membership.tenant_id
  from event
  join event_target_cohort on event_id=event.id
  join cohort_membership on event_target_cohort.cohort_id=cohort_membership.cohort_id and active_range @> now()
  left join event_registration on target_cohort_id=event_target_cohort.id and event_registration.person_id=cohort_membership.person_id and event_registration.event_id=event.id
  where event_registration.id is null
    and exists (select 1 from event_instance where event_id=event.id and until > now())
    and cohort_membership.id = c.id
    and event.tenant_id = c.tenant_id
  on conflict on constraint event_registration_unique_event_person_couple_key do nothing
  returning event_registration.*;
$$;
