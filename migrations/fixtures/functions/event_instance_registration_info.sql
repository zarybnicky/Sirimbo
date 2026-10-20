drop function if exists event_instance_registration_info;
drop type if exists event_instance_registration_info;

create type event_instance_registration_info as (
  registrations integer,
  people integer,
  remaining_capacity integer,
  my boolean
);

comment on type event_instance_registration_info is '@name event_registration_info';

create or replace function event_instance_registration_info(inst event_instance)
  returns event_instance_registration_info
  language sql stable security definer
  set search_path = public, pg_catalog, pg_temp
as $$
  select row(
    count(*) filter (where parent_registration_id is null)::integer,
    count(person_id)::integer,
    case
      when inst.capacity is null or inst.capacity <= 0 then null
      else inst.capacity
        - case inst.capacity_unit
            when 'people' then count(person_id)::integer
            when 'registrations' then count(*) filter (where parent_registration_id is null)::integer
          end
    end,
    count(*) filter (where person_id = any(current_person_ids())) > 0
  )::event_instance_registration_info
  from event_instance_registration
  where instance_id = inst.id and registration_status = 'active';
$$;

grant all on function event_instance_registration_info to anonymous;
