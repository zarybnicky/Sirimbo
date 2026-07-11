CREATE FUNCTION public.detach_event_instance(p_instance_id bigint, p_new_event_name text DEFAULT NULL::text) RETURNS public.event
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'pg_temp'
    AS $$
declare
  v_tenant_id bigint;
  v_old_event_id bigint;
  v_new_event_id bigint;
  v_result public.event;
begin
  select instance.tenant_id, instance.event_id
  into v_tenant_id, v_old_event_id
  from public.event_instance instance
  where instance.id = p_instance_id
  for update;

  if not found or v_old_event_id is null then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '22023';
  end if;
  if v_tenant_id <> public.current_tenant_id() then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  if not (
    coalesce(app_private.is_system_admin(public.current_user_id()), false)
    or exists (
      select 1
      from public.current_tenant_administrator administrator
      where administrator.person_id = any (
        coalesce(public.current_person_ids(), '{}'::bigint[])
      )
        and administrator.active_range @> now()
    )
    or (
      exists (
        select 1
        from public.current_tenant_trainer trainer
        where trainer.person_id = any (
          coalesce(public.current_person_ids(), '{}'::bigint[])
        )
          and trainer.active_range @> now()
      )
      and app_private.can_trainer_edit_instance(p_instance_id)
    )
  ) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  perform 1
  from public.event event
  where event.id = v_old_event_id
    and event.tenant_id = v_tenant_id
  for update;

  if not exists (
    select 1
    from public.event_instance sibling
    where sibling.event_id = v_old_event_id
      and sibling.id <> p_instance_id
  ) then
    raise exception 'CANNOT_DETACH_ONLY_INSTANCE' using errcode = '22023';
  end if;

  insert into public.event (
    name, location_text, description, capacity, files_legacy,
    is_locked, is_visible, summary, is_public, enable_notes, tenant_id,
    type, location_id
  )
  select coalesce(p_new_event_name, event.name), event.location_text,
    event.description, event.capacity, event.files_legacy, event.is_locked,
    event.is_visible, event.summary, event.is_public, event.enable_notes,
    event.tenant_id, event.type, event.location_id
  from public.event event
  where event.id = v_old_event_id
  returning id into v_new_event_id;

  -- The exact instance lists are authoritative; these rows only keep the
  -- legacy event-shaped API working during the transition.
  insert into public.event_target_cohort (tenant_id, event_id, cohort_id)
  select v_tenant_id, v_new_event_id, cohort_id
  from (
    select target.cohort_id
    from public.event_instance_target_cohort target
    where target.instance_id = p_instance_id
    union
    select registration.target_cohort_id
    from public.event_instance_registration registration
    where registration.instance_id = p_instance_id
      and registration.parent_registration_id is null
      and registration.target_cohort_id is not null
  ) cohort;

  insert into public.event_trainer (
    tenant_id, event_id, person_id, lessons_offered
  )
  select trainer.tenant_id, v_new_event_id, trainer.person_id,
    trainer.lessons_offered
  from public.event_instance_trainer trainer
  where trainer.instance_id = p_instance_id
  union all
  select trainer.tenant_id, v_new_event_id, trainer.person_id,
    trainer.lessons_offered
  from public.event_trainer trainer
  where trainer.event_id = v_old_event_id
    and not exists (
      select 1
      from public.event_instance_trainer
      where instance_id = p_instance_id
    );

  insert into public.event_registration (
    tenant_id, event_id, target_cohort_id, couple_id, person_id, note
  )
  select exact.tenant_id, v_new_event_id, target.id,
    legacy.couple_id, legacy.person_id, exact.note
  from public.event_instance_registration exact
  join public.event_registration legacy
    on legacy.id = exact.legacy_registration_id
  left join public.event_target_cohort target
    on target.event_id = v_new_event_id
   and target.cohort_id = exact.target_cohort_id
  where exact.instance_id = p_instance_id
    and exact.parent_registration_id is null
    and exact.legacy_registration_id is not null;

  -- The existing bridge and unit keys make this an unambiguous in-place rekey.
  update public.event_instance_registration exact
  set event_id = v_new_event_id,
      legacy_registration_id = replacement.id
  from public.event_registration legacy
  join public.event_registration replacement
    on replacement.event_id = v_new_event_id
   and replacement.person_id is not distinct from legacy.person_id
   and replacement.couple_id is not distinct from legacy.couple_id
  where exact.instance_id = p_instance_id
    and exact.legacy_registration_id = legacy.id
    and legacy.event_id = v_old_event_id;

  update public.event_instance_registration exact
  set event_id = v_new_event_id
  where exact.instance_id = p_instance_id
    and exact.legacy_registration_id is null
    and exact.event_id is distinct from v_new_event_id;

  update public.event_instance instance
  set event_id = v_new_event_id
  where instance.id = p_instance_id;

  update public.event_instance_trainer trainer
  set event_id = v_new_event_id
  where trainer.instance_id = p_instance_id
    and trainer.event_id is distinct from v_new_event_id;

  update public.event_lesson_demand demand
  set event_id = v_new_event_id
  from public.event_instance_registration registration
  where registration.id = demand.registration_id
    and registration.instance_id = p_instance_id
    and demand.event_id is distinct from v_new_event_id;

  select event.* into strict v_result
  from public.event event
  where event.id = v_new_event_id;

  return v_result;
end;
$$;

REVOKE ALL ON FUNCTION public.detach_event_instance(p_instance_id bigint, p_new_event_name text) FROM PUBLIC;
GRANT ALL ON FUNCTION public.detach_event_instance(p_instance_id bigint, p_new_event_name text) TO trainer;
