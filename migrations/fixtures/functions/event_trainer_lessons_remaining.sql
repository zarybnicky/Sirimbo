create or replace function event_trainer_lessons_remaining(e event_trainer) returns integer as $$
  select case
    when e.lessons_offered is null then null
    else e.lessons_offered - (
      select coalesce(sum(demand.lesson_count), 0)
      from event_lesson_request demand
      join event_registration registration
        on registration.id = demand.registration_id
       and registration.registration_status = 'active'
      where demand.trainer_id = e.id
    )
  end;
$$ language sql stable security definer set search_path = pg_catalog, public, pg_temp;

grant all on function event_trainer_lessons_remaining to anonymous;
