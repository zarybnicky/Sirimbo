
CREATE or replace FUNCTION event_instance_trainer_lessons_remaining(e event_instance_trainer) RETURNS integer AS $$
  select case
    when e.lessons_offered is null then null
    else e.lessons_offered - (
      select coalesce(sum(demand.lesson_count), 0)
      from event_lesson_demand demand
      join event_instance_registration registration
        on registration.id = demand.registration_id
       and registration.registration_status = 'active'
      where demand.trainer_id = e.id
    )
  end;
$$ LANGUAGE sql STABLE;

CREATE or replace FUNCTION event_instance_remaining_person_spots(inst event_instance) RETURNS integer AS $$
  select case
    when inst.capacity is null or inst.capacity <= 0 then null
    else inst.capacity - case inst.capacity_unit
        when 'people' then (
          select count(*)::integer
          from event_instance_registration registration
          where registration.instance_id = inst.id
            and registration.person_id is not null
            and registration.registration_status = 'active'
            and registration.status <> 'cancelled'
        )
        when 'registrations' then (
          select count(*)::integer
          from event_instance_registration registration
          where registration.instance_id = inst.id
            and registration.parent_registration_id is null
            and registration.registration_status = 'active'
        )
      end - (
        select count(*)::integer
        from event_external_registration external_registration
        where external_registration.instance_id = inst.id
      )
  end;
$$ LANGUAGE sql STABLE security definer set search_path = public, pg_catalog, pg_temp;

GRANT ALL ON FUNCTION event_instance_trainer_lessons_remaining(e event_instance_trainer) TO anonymous;
GRANT ALL ON FUNCTION event_instance_remaining_person_spots(inst event_instance) TO anonymous;
