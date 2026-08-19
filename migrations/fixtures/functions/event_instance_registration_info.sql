drop function if exists event_instance_registration_info;
drop type if exists event_instance_registration_info;

create type event_instance_registration_info as (
  registrations integer,
  people integer,
  remaining_capacity integer,
  my boolean
);

create or replace function event_instance_registration_info(inst event_instance)
  returns event_instance_registration_info
  language sql stable security definer
  set search_path = public, pg_catalog, pg_temp
as $$
  with registration_counts as (
    select
      count(*) filter (where parent_registration_id is null)::integer
        as registrations,
      count(*) filter (where person_id is not null)::integer
        as people,
      count(*) filter (where person_id = any(current_person_ids())) > 0 as my
    from event_instance_registration
    where instance_id = inst.id and registration_status = 'active'
  ), external_counts as (
    select count(*)::integer as registrations
    from event_external_registration
    where instance_id = inst.id
  )
  select row(
    registrations.registrations + external.registrations,
    registrations.people + external.registrations,
    case
      when inst.capacity is null or inst.capacity <= 0 then null
      else inst.capacity
        - case inst.capacity_unit
            when 'people' then registrations.people
            when 'registrations' then registrations.registrations
          end
        - external.registrations
    end,
    registrations.my
  )::event_instance_registration_info
  from registration_counts registrations
  cross join external_counts external;
$$;

grant all on function event_instance_registration_info to anonymous;
