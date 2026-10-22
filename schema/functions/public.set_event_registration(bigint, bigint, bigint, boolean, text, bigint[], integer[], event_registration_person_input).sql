CREATE FUNCTION public.set_event_registration(event_id bigint, person_id bigint, couple_id bigint, is_registered boolean, note text DEFAULT NULL::text, lesson_trainer_ids bigint[] DEFAULT NULL::bigint[], lesson_counts integer[] DEFAULT NULL::integer[], person public.event_registration_person_input DEFAULT NULL::public.event_registration_person_input) RETURNS public.event_registration
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $_$
declare
  target_instance event;
  registration event_registration;
  registration_found boolean;
  is_self boolean;
  is_manager boolean;
  registration_source event_registration_source;
  required_capacity integer;
  remaining_capacity integer;
  request record;
begin
  if is_registered is null or num_nonnulls(person_id, couple_id, person) <> 1
    or (person is distinct from null and not is_registered) then
    raise exception 'INVALID_REGISTRANT' using errcode = '22023';
  end if;

  if num_nonnulls(lesson_trainer_ids, lesson_counts) = 1
    or cardinality(lesson_trainer_ids) <> cardinality(lesson_counts)
    or exists (
      select 1
      from unnest(lesson_trainer_ids, lesson_counts) demand(trainer_id, lesson_count)
      where demand.trainer_id is null
        or demand.lesson_count is null
        or demand.lesson_count < 0
    ) then
    raise exception 'INVALID_LESSON_DEMANDS' using errcode = '22023';
  end if;
  if not is_registered and lesson_trainer_ids is not null then
    raise exception 'INVALID_LESSON_DEMANDS' using errcode = '22023';
  end if;

  select * into target_instance
  from event
  where id = event_id
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
    ) and app_private.can_trainer_edit_event(event_id)
  );

  if person is distinct from null then
    if nullif(btrim(person.first_name), '') is null
      or nullif(btrim(person.last_name), '') is null
      or person.gender is null
      or nullif(btrim(person.nationality), '') is null
      or person.email is null or person.email !~ '^[^[:space:]@]+@[^[:space:]@]+\.[^[:space:]@]+$'
      or nullif(btrim(person.phone), '') is null
      or person.birth_date > current_date
      or person.csts_id <= 0 or person.wdsf_id <= 0 then
      raise exception 'INVALID_PERSON_DETAILS' using errcode = '22023';
    end if;

    select r.* into registration
    from event_registration r
    join person p on p.id = r.person_id
    where r.event_id = set_event_registration.event_id
      and r.tenant_id = current_tenant_id()
      and r.created_by is not distinct from current_user_id()
      and r.registration_status = 'active'
      and r.parent_registration_id is null
      and r.note is not distinct from set_event_registration.note
      and row(
        p.first_name, p.last_name, p.gender, p.nationality,
        p.email, p.phone, p.birth_date, p.csts_id, p.wdsf_id
      ) is not distinct from row(
        btrim((set_event_registration.person).first_name), btrim((set_event_registration.person).last_name),
        (set_event_registration.person).gender, (set_event_registration.person).nationality,
        btrim((set_event_registration.person).email), btrim((set_event_registration.person).phone),
        (set_event_registration.person).birth_date, (set_event_registration.person).csts_id,
        (set_event_registration.person).wdsf_id
      )
      and coalesce((
        select jsonb_object_agg(d.trainer_id, d.lesson_count)
        from event_lesson_request d where d.registration_id = r.id
      ), '{}'::jsonb) = coalesce((
        select jsonb_object_agg(d.trainer_id, d.lesson_count)
        from unnest(lesson_trainer_ids, lesson_counts) d(trainer_id, lesson_count)
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
      btrim((set_event_registration.person).first_name), btrim((set_event_registration.person).last_name),
      (set_event_registration.person).gender, (set_event_registration.person).nationality,
      btrim((set_event_registration.person).email), btrim((set_event_registration.person).phone),
      (set_event_registration.person).birth_date, (set_event_registration.person).csts_id,
      (set_event_registration.person).wdsf_id
    )
    returning id into person_id;

    is_self := not is_manager;
  else
    is_self := coalesce(person_id = any(current_person_ids()), false)
      or coalesce(couple_id = any(current_couple_ids()), false);
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
  from event_registration r
  where r.event_id = set_event_registration.event_id
    and r.person_id is not distinct from set_event_registration.person_id
    and r.couple_id is not distinct from set_event_registration.couple_id
  order by (r.registration_status = 'active') desc, (r.parent_registration_id is null) desc, r.id desc
  limit 1;
  registration_found := found;

  if not is_registered then
    if not registration_found then
      raise exception 'REGISTRATION_NOT_FOUND' using errcode = '22023';
    end if;

    update event_registration
    set registration_status = 'cancelled',
        target_cohort_id = null,
        source = registration_source
    where id = registration.id or parent_registration_id = registration.id;

    select * into registration from event_registration where id = registration.id;
    return registration;
  end if;

  if registration_found and registration.registration_status = 'active' then
    if note is not null then
      update event_registration set note = set_event_registration.note where id = registration.id
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
      remaining_capacity := event_remaining_person_spots(target_instance);
      required_capacity := case
        when target_instance.capacity_unit = 'people' and couple_id is not null then 2
        when target_instance.capacity_unit = 'registrations' and exists (
          select 1 from event_registration parent
          where parent.id = registration.parent_registration_id and parent.registration_status = 'active'
        ) then 0
        else 1
      end;
      if couple_id is not null then
        required_capacity := required_capacity - (
          select count(*)::integer from event_registration r
          join couple c on c.id = set_event_registration.couple_id and r.person_id in (c.man_id, c.woman_id)
          where r.event_id = set_event_registration.event_id and r.registration_status = 'active'
            and (target_instance.capacity_unit = 'people' or r.parent_registration_id is null)
        );
      end if;
      if remaining_capacity is not null and remaining_capacity < required_capacity then
        raise exception 'CAPACITY_EXCEEDED' using errcode = '22023';
      end if;
    end if;

    if registration_found then
      update event_registration
      set registration_status = 'active',
          target_cohort_id = null,
          source = registration_source,
          parent_registration_id = (
            select parent.id from event_registration parent
            where parent.id = registration.parent_registration_id and parent.registration_status = 'active'
          )
      where id = registration.id;

      if note is not null then
        update event_registration set note = set_event_registration.note where id = registration.id;
      end if;
    else
      insert into event_registration (
        event_id, person_id, couple_id, source, status, note
      ) values (
        event_id, set_event_registration.person_id, set_event_registration.couple_id, registration_source,
        case when set_event_registration.person_id is not null then 'unknown'::attendance_type end,
        set_event_registration.note
      ) returning * into registration;
    end if;
  end if;

  if couple_id is not null then
    perform app_private.ensure_couple_person_registrations(registration.id);
  end if;

  select * into registration from event_registration where id = registration.id;

  if lesson_trainer_ids is not null then
    delete from event_lesson_request
    where registration_id = registration.id
      and trainer_id <> all(lesson_trainer_ids);

    for request in
      select demand.trainer_id, demand.lesson_count
      from unnest(lesson_trainer_ids, lesson_counts)
        demand(trainer_id, lesson_count)
      order by demand.trainer_id
    loop
      perform app_private.set_lesson_request(registration, request.trainer_id, request.lesson_count);
    end loop;
  end if;

  if person is distinct from null then
    insert into person_invitation (person_id, email, event_id)
    values (registration.person_id, btrim((set_event_registration.person).email), event_id);
  end if;

  return registration;
end;
$_$;

GRANT ALL ON FUNCTION public.set_event_registration(event_id bigint, person_id bigint, couple_id bigint, is_registered boolean, note text, lesson_trainer_ids bigint[], lesson_counts integer[], person public.event_registration_person_input) TO anonymous;
