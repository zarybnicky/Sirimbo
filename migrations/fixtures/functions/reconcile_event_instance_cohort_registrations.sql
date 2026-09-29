drop function if exists app_private.reconcile_event_instance_cohort_registrations(bigint[], bigint[], bigint[]);

create or replace function app_private.reconcile_event_instance_cohort_registrations(
  p_instance_ids bigint[],
  p_person_ids bigint[] default null
) returns void
  language sql
  security invoker
  set search_path = pg_catalog, public, pg_temp
as $$
  select instance.id from event_instance instance
  where instance.id = any(p_instance_ids) order by instance.id for update;

  -- Direct registrations (including cancellations) take precedence. Couples
  -- already supply their person rows; cohorts supply only the remaining people.
  with desired as materialized (
    select instance.id as instance_id, instance.tenant_id, membership.person_id,
      min(target.cohort_id) as target_cohort_id
    from event_instance instance
    join event_instance_target_cohort target on target.instance_id = instance.id
    join cohort_membership membership on membership.cohort_id = target.cohort_id
    where instance.id = any(p_instance_ids)
      and instance.since >= now()
      and membership.status = 'active' and membership.active_range @> now()
      and (p_person_ids is null or membership.person_id = any(p_person_ids))
      and not exists (
        select 1 from event_instance_registration registration
        where registration.instance_id = instance.id
          and registration.person_id = membership.person_id
          and (
            (registration.parent_registration_id is null and registration.source is distinct from 'cohort')
            or (registration.parent_registration_id is not null and registration.registration_status = 'active')
          )
      )
    group by instance.id, instance.tenant_id, membership.person_id
  ), cancelled as (
    update event_instance_registration registration
    set registration_status = 'cancelled'
    from event_instance instance
    where registration.instance_id = any(p_instance_ids)
      and instance.id = registration.instance_id and instance.since >= now()
      and registration.source = 'cohort'
      and registration.registration_status = 'active'
      and (p_person_ids is null or registration.person_id = any(p_person_ids))
      and not exists (
        select 1 from desired
        where desired.instance_id = registration.instance_id
          and desired.person_id = registration.person_id
      )
  )
  insert into event_instance_registration as registration (
    tenant_id, instance_id, person_id, target_cohort_id, source, status
  )
  select tenant_id, instance_id, person_id, target_cohort_id, 'cohort', 'unknown'
  from desired
  on conflict (instance_id, couple_id, person_id) where parent_registration_id is null
  do update set registration_status = 'active', target_cohort_id = excluded.target_cohort_id
  where registration.source = 'cohort'
    and (registration.registration_status, registration.target_cohort_id)
      is distinct from ('active'::event_instance_registration_status, excluded.target_cohort_id);
$$;

revoke all on function app_private.reconcile_event_instance_cohort_registrations from public;
grant execute on function app_private.reconcile_event_instance_cohort_registrations to trainer, administrator;

drop trigger if exists _500_reconcile_registrations on event_instance_target_cohort;
drop function if exists app_private.tg_event_instance_target_cohort__reconcile();
