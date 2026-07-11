create or replace function public.detach_event_instance(
  p_instance_id bigint,
  p_new_event_name text default null
)
  returns public.event
  language plpgsql
  security definer
  set search_path = pg_catalog, pg_temp
as $$
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

  insert into public.event_trainer (
    tenant_id, event_id, person_id, lessons_offered
  )
  select trainer.tenant_id, v_new_event_id, trainer.person_id,
    trainer.lessons_offered
  from public.event_instance_trainer trainer
  where trainer.instance_id = p_instance_id;

  update public.event_instance_registration exact
  set event_id = v_new_event_id,
      legacy_registration_id = null
  where exact.instance_id = p_instance_id
    and (exact.event_id is distinct from v_new_event_id
      or exact.legacy_registration_id is not null);

  update public.event_instance instance
  set event_id = v_new_event_id,
      series_id = null
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

select verify_function('public.detach_event_instance');
revoke execute on function public.detach_event_instance(bigint, text)
  from public, anonymous, member;
grant execute on function public.detach_event_instance(bigint, text)
  to trainer;
