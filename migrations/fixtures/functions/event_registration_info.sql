drop function if exists event_registration_info;
drop type if exists event_registration_info;

create type event_registration_info as (
  registrations integer,
  people integer,
  remaining_capacity integer,
  my boolean,
  can_register boolean
);

create or replace function event_registration_info(e event)
  returns event_registration_info
  language sql stable security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  with counts as (
    select
      count(*) filter (where parent_registration_id is null)::integer as registrations,
      count(person_id)::integer as people,
      count(*) filter (where person_id = any(current_person_ids())) > 0 as my
    from event_registration
    where event_id = e.id and registration_status = 'active'
  ), capacity as (
    select counts.*, case
      when e.capacity is null or e.capacity <= 0 then null
      else e.capacity
        - case e.capacity_unit
            when 'people' then people
            when 'registrations' then registrations
          end
    end as remaining_capacity
    from counts
  )
  select row(
    registrations, people, remaining_capacity, my,
    coalesce(
          not coalesce(e.is_locked, false)
      and not e.is_cancelled
      and e.until > now()
      and coalesce(remaining_capacity > 0, true),
      false
    )
  )::event_registration_info
  from capacity;
$$;

grant all on function event_registration_info to anonymous;
