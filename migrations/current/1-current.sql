
create or replace function event_instance_registrations(inst event_instance)
  returns setof event_registration language sql stable as $$
  select * from event_registration where event_id = inst.event_id;
$$;
grant all on function event_instance_registrations to anonymous;

create or replace function event_instance_my_registrations(inst event_instance)
  returns setof event_registration language sql stable as $$
  select * from event_registration
  where event_id = inst.event_id
  and (person_id = any (current_person_ids())
    or couple_id = any (current_couple_ids()));
$$;
comment on function event_instance_my_registrations is '@simpleCollections only';
grant all on function event_instance_my_registrations to anonymous;

create or replace function event_instance_target_cohorts(inst event_instance)
  returns setof event_target_cohort language sql stable as $$
  select * from event_target_cohort where event_id = inst.event_id;
$$;
comment on function event_instance_target_cohorts is '@simpleCollections only';
grant all on function event_instance_target_cohorts to anonymous;

create or replace function event_instance_remaining_person_spots(inst event_instance)
  returns integer language sql stable security definer as $$
  select inst.capacity - (
    select coalesce(sum(case when couple_id is not null then 2 else 1 end), 0)
    from event_registration where event_id = inst.event_id
  ) - (
    select coalesce(count(id), 0)
    from event_external_registration where event_id = inst.event_id
  );
$$;
grant all on function event_instance_remaining_person_spots to anonymous;
