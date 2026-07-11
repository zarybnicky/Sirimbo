CREATE FUNCTION app_private.reconcile_event_instance_cohort_registrations(p_instance_ids bigint[], p_person_ids bigint[] DEFAULT NULL::bigint[]) RETURNS void
    LANGUAGE sql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select instance.id
  from event_instance instance
  where instance.id = any (p_instance_ids)
  order by instance.id
  for update;

  with desired as (
    select target.instance_id, membership.person_id,
      min(target.cohort_id) as target_cohort_id
    from event_instance_target_cohort target
    join event_instance instance on instance.id = target.instance_id
    join cohort_membership membership on membership.cohort_id = target.cohort_id
    where target.instance_id = any (p_instance_ids)
      and instance.since >= now()
      and membership.status = 'active'
      and membership.active_range @> now()
      and (p_person_ids is null or membership.person_id = any (p_person_ids))
    group by target.instance_id, membership.person_id
  )
  update event_instance_registration r
  set registration_status = 'active',
      source = 'cohort',
      target_cohort_id = desired.target_cohort_id
  from desired
  where r.instance_id = desired.instance_id
    and r.person_id = desired.person_id
    and r.parent_registration_id is null
    and r.source = 'cohort'
    and not exists (
      select 1
      from event_instance_registration attendee
      where attendee.instance_id = desired.instance_id
        and attendee.person_id = desired.person_id
        and attendee.registration_status = 'active'
        and coalesce(attendee.parent_registration_id, attendee.id) <> r.id
    )
    and (r.registration_status, r.target_cohort_id)
      is distinct from (
        'active'::event_instance_registration_status,
        desired.target_cohort_id
      );

  with desired as (
    select target.instance_id, instance.tenant_id, membership.person_id,
      min(target.cohort_id) as target_cohort_id
    from event_instance_target_cohort target
    join event_instance instance on instance.id = target.instance_id
    join cohort_membership membership on membership.cohort_id = target.cohort_id
    where target.instance_id = any (p_instance_ids)
      and instance.since >= now()
      and membership.status = 'active'
      and membership.active_range @> now()
      and (p_person_ids is null or membership.person_id = any (p_person_ids))
    group by target.instance_id, instance.tenant_id, membership.person_id
  )
  insert into event_instance_registration (
    tenant_id, instance_id, person_id, target_cohort_id,
    source, status
  )
  select desired.tenant_id, desired.instance_id, desired.person_id,
    desired.target_cohort_id, 'cohort', 'unknown'
  from desired
  where not exists (
    select 1
    from event_instance_registration r
    where r.instance_id = desired.instance_id
      and r.person_id = desired.person_id
      and r.parent_registration_id is null
  ) and not exists (
    select 1
    from event_instance_registration attendee
    where attendee.instance_id = desired.instance_id
      and attendee.person_id = desired.person_id
      and attendee.registration_status = 'active'
  );

  update event_instance_registration r
  set registration_status = 'cancelled'
  from event_instance instance
  where r.instance_id = instance.id
    and r.instance_id = any (p_instance_ids)
    and r.parent_registration_id is null
    and r.person_id is not null
    and r.source = 'cohort'
    and r.registration_status = 'active'
    and instance.since >= now()
    and (p_person_ids is null or r.person_id = any (p_person_ids))
    and not exists (
      select 1
      from event_instance_target_cohort target
      join cohort_membership membership
        on membership.cohort_id = target.cohort_id
       and membership.person_id = r.person_id
      where target.instance_id = r.instance_id
        and membership.status = 'active'
        and membership.active_range @> now()
    );
$$;

REVOKE ALL ON FUNCTION app_private.reconcile_event_instance_cohort_registrations(p_instance_ids bigint[], p_person_ids bigint[]) FROM PUBLIC;
