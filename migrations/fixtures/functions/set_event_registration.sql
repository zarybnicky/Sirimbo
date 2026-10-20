drop function if exists set_event_registration;
drop function if exists set_event_instance_registration;
drop function if exists register_to_event;
drop function if exists app_private.set_event_instance_registration;
drop type if exists event_registration_person_input;

create type event_registration_person_input as (
  first_name text,
  last_name text,
  gender gender_type,
  nationality text,
  email text,
  phone text,
  birth_date date,
  csts_id integer,
  wdsf_id integer
);

create function set_event_registration(
  p_instance_id bigint,
  p_person_id bigint,
  p_couple_id bigint,
  p_is_registered boolean,
  p_note text default null,
  p_lesson_trainer_ids bigint[] default null,
  p_lesson_counts integer[] default null,
  p_person event_registration_person_input default null
) returns event_instance_registration
language plpgsql security definer
set search_path = pg_catalog, public, pg_temp
as $$
declare
  target_instance event_instance;
  registration event_instance_registration;
  registration_found boolean;
  is_self boolean;
  is_manager boolean;
  registration_source event_registration_source;
  required_capacity integer;
  remaining_capacity integer;
  request record;
begin
  if p_is_registered is null or num_nonnulls(p_person_id, p_couple_id, p_person) <> 1
    or (p_person is distinct from null and not p_is_registered) then
    raise exception 'INVALID_REGISTRANT' using errcode = '22023';
  end if;

  if num_nonnulls(p_lesson_trainer_ids, p_lesson_counts) = 1
    or cardinality(p_lesson_trainer_ids) <> cardinality(p_lesson_counts)
    or exists (
      select 1
      from unnest(p_lesson_trainer_ids, p_lesson_counts) demand(trainer_id, lesson_count)
      where demand.trainer_id is null
        or demand.lesson_count is null
        or demand.lesson_count < 0
    ) then
    raise exception 'INVALID_LESSON_DEMANDS' using errcode = '22023';
  end if;
  if not p_is_registered and p_lesson_trainer_ids is not null then
    raise exception 'INVALID_LESSON_DEMANDS' using errcode = '22023';
  end if;

  select * into target_instance
  from event_instance
  where id = p_instance_id
    and tenant_id = current_tenant_id()
  for update;

  if not found then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '22023';
  end if;

  is_manager := pg_has_role(
    coalesce(nullif(current_setting('role'), 'none'), session_user), 'administrator', 'member'
  ) or (
    pg_has_role(
      coalesce(nullif(current_setting('role'), 'none'), session_user), 'trainer', 'member'
    ) and app_private.can_trainer_edit_instance(p_instance_id)
  );

  if p_person is distinct from null then
    if nullif(btrim(p_person.first_name), '') is null
      or nullif(btrim(p_person.last_name), '') is null
      or p_person.gender is null
      or nullif(btrim(p_person.nationality), '') is null
      or p_person.email is null or p_person.email !~ '^[^[:space:]@]+@[^[:space:]@]+\.[^[:space:]@]+$'
      or nullif(btrim(p_person.phone), '') is null
      or p_person.birth_date > current_date
      or p_person.csts_id <= 0 or p_person.wdsf_id <= 0 then
      raise exception 'INVALID_PERSON_DETAILS' using errcode = '22023';
    end if;

    select r.* into registration
    from event_instance_registration r
    join person p on p.id = r.person_id
    where r.instance_id = p_instance_id
      and r.tenant_id = current_tenant_id()
      and r.created_by is not distinct from current_user_id()
      and r.registration_status = 'active'
      and r.parent_registration_id is null
      and r.note is not distinct from p_note
      and row(
        p.first_name, p.last_name, p.gender, p.nationality,
        p.email, p.phone, p.birth_date, p.csts_id, p.wdsf_id
      ) is not distinct from row(
        btrim(p_person.first_name), btrim(p_person.last_name), p_person.gender,
        p_person.nationality, btrim(p_person.email), btrim(p_person.phone),
        p_person.birth_date, p_person.csts_id, p_person.wdsf_id
      )
      and coalesce((
        select jsonb_object_agg(d.trainer_id, d.lesson_count)
        from event_lesson_demand d where d.registration_id = r.id
      ), '{}'::jsonb) = coalesce((
        select jsonb_object_agg(d.trainer_id, d.lesson_count)
        from unnest(p_lesson_trainer_ids, p_lesson_counts) d(trainer_id, lesson_count)
        where d.lesson_count > 0
      ), '{}'::jsonb)
    order by r.id
    limit 1;

    if found then
      return registration;
    end if;

    insert into person (
      first_name, last_name, gender, nationality, email, phone, birth_date, csts_id, wdsf_id
    ) values (
      btrim(p_person.first_name), btrim(p_person.last_name), p_person.gender,
      p_person.nationality, btrim(p_person.email), btrim(p_person.phone),
      p_person.birth_date, p_person.csts_id, p_person.wdsf_id
    )
    returning id into p_person_id;

    is_self := not is_manager;
  else
    is_self := coalesce(p_person_id = any(current_person_ids()), false)
      or coalesce(p_couple_id = any(current_couple_ids()), false);
  end if;

  if not is_self and not is_manager then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;
  if target_instance.is_locked and not is_manager then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  registration_source := case when is_self
    then 'self'::event_registration_source
    else 'manager'::event_registration_source
  end;

  select * into registration
  from event_instance_registration
  where instance_id = p_instance_id
    and person_id is not distinct from p_person_id
    and couple_id is not distinct from p_couple_id
  order by (registration_status = 'active') desc, (parent_registration_id is null) desc, id desc
  limit 1;
  registration_found := found;

  if not p_is_registered then
    if not registration_found then
      raise exception 'REGISTRATION_NOT_FOUND' using errcode = '22023';
    end if;

    update event_instance_registration
    set registration_status = 'cancelled',
        target_cohort_id = null,
        source = registration_source
    where id = registration.id or parent_registration_id = registration.id;

    select * into registration from event_instance_registration where id = registration.id;
    return registration;
  end if;

  if registration_found and registration.registration_status = 'active' then
    if p_note is not null then
      update event_instance_registration set note = p_note where id = registration.id
      returning * into registration;
    end if;
  else
    if not is_manager and (
      target_instance.is_cancelled
        or target_instance.until <= now()
        or not (
          coalesce(target_instance.is_public, false)
          or (
            coalesce(target_instance.is_visible, false)
            and current_tenant_id() = any(my_tenants_array())
          )
        )
    ) then
      raise exception 'NOT_ALLOWED' using errcode = '28000';
    end if;

    if not is_manager then
      remaining_capacity := event_instance_remaining_person_spots(target_instance);
      required_capacity := case
        when target_instance.capacity_unit = 'people' and p_couple_id is not null then 2
        when target_instance.capacity_unit = 'registrations' and exists (
          select 1 from event_instance_registration parent
          where parent.id = registration.parent_registration_id and parent.registration_status = 'active'
        ) then 0
        else 1
      end;
      if p_couple_id is not null then
        required_capacity := required_capacity - (
          select count(*)::integer from event_instance_registration r
          join couple c on c.id = p_couple_id and r.person_id in (c.man_id, c.woman_id)
          where r.instance_id = p_instance_id and r.registration_status = 'active'
            and (target_instance.capacity_unit = 'people' or r.parent_registration_id is null)
        );
      end if;
      if remaining_capacity is not null and remaining_capacity < required_capacity then
        raise exception 'CAPACITY_EXCEEDED' using errcode = '22023';
      end if;
    end if;

    if registration_found then
      update event_instance_registration
      set registration_status = 'active',
          target_cohort_id = null,
          source = registration_source,
          parent_registration_id = (
            select parent.id from event_instance_registration parent
            where parent.id = registration.parent_registration_id and parent.registration_status = 'active'
          )
      where id = registration.id;

      if p_note is not null then
        update event_instance_registration set note = p_note where id = registration.id;
      end if;
    else
      insert into event_instance_registration (
        instance_id, person_id, couple_id, source, status, note
      ) values (
        p_instance_id, p_person_id, p_couple_id, registration_source,
        case when p_person_id is not null then 'unknown'::attendance_type end,
        p_note
      ) returning * into registration;
    end if;
  end if;

  if p_couple_id is not null then
    perform app_private.ensure_couple_person_registrations(registration.id);
  end if;

  select * into registration from event_instance_registration where id = registration.id;

  if p_lesson_trainer_ids is not null then
    delete from event_lesson_demand
    where registration_id = registration.id
      and trainer_id <> all(p_lesson_trainer_ids);

    for request in
      select demand.trainer_id, demand.lesson_count
      from unnest(p_lesson_trainer_ids, p_lesson_counts)
        demand(trainer_id, lesson_count)
      order by demand.trainer_id
    loop
      perform app_private.set_lesson_request(registration, request.trainer_id, request.lesson_count);
    end loop;
  end if;

  if p_person is distinct from null then
    insert into person_invitation (person_id, email, instance_id)
    values (registration.person_id, btrim(p_person.email), p_instance_id);
  end if;

  return registration;
end;
$$;

grant execute on function set_event_registration to anonymous;
