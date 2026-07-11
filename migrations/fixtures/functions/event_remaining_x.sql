
CREATE or replace FUNCTION event_trainer_lessons_remaining(e event_trainer) RETURNS integer AS $$
  select case
    when e.lessons_offered is null then null
    else e.lessons_offered - (
      select coalesce(sum(demand.lesson_count), 0)
      from event_lesson_demand demand
      join event_instance_registration registration
        on registration.id = demand.registration_id
       and registration.registration_status = 'active'
      join event_instance_trainer trainer on trainer.id = demand.trainer_id
      join event_instance instance on instance.id = trainer.instance_id
      where instance.event_id = e.event_id and trainer.person_id = e.person_id
    )
  end;
$$ LANGUAGE sql STABLE;

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

CREATE or replace FUNCTION event_remaining_person_spots(e event) RETURNS integer AS $$
  select e.capacity - (
    select coalesce(sum(case when couple_id is not null then 2 else 1 end), 0)
    from event_registration where event_id = e.id
  ) - (
    select coalesce(count(id), 0)
    from event_external_registration where event_id = e.id
  );
$$ LANGUAGE sql STABLE security definer;

CREATE or replace FUNCTION event_remaining_lessons(e event) RETURNS integer AS $$
  select case
    when exists (
      select 1 from event_trainer et
      where et.event_id = e.id and et.lessons_offered is null
    ) then null
    else (
      select coalesce(sum(lessons_offered), 0) from event_trainer et where et.event_id = e.id
    ) - (
      select coalesce(sum(demand.lesson_count), 0)
      from event_lesson_demand demand
      join event_instance_registration registration
        on registration.id = demand.registration_id
       and registration.registration_status = 'active'
      where demand.event_id = e.id
    )
  end;
$$ LANGUAGE sql STABLE security definer;

GRANT ALL ON FUNCTION event_trainer_lessons_remaining(e event_trainer) TO anonymous;
GRANT ALL ON FUNCTION event_instance_trainer_lessons_remaining(e event_instance_trainer) TO anonymous;
GRANT ALL ON FUNCTION event_remaining_person_spots(e event) TO anonymous;
GRANT ALL ON FUNCTION event_remaining_lessons(e event) TO anonymous;
