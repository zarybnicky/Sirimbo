CREATE FUNCTION public.event_registration_info(e public.event) RETURNS public.event_registration_info
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  with counts as (
    select
      count(*) filter (where parent_registration_id is null)::integer as registrations,
      count(person_id)::integer as people,
      count(*) filter (where person_id = any(current_person_ids())) > 0 as my
    from event_registration
    where event_id = e.id and registration_status = 'active'
  ), capacity as (
    select counts.*, coalesce(e.registration_period @> now(), false) as registration_open, case
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
          registration_open
      and not e.is_cancelled
      and e.until > now()
      and coalesce(remaining_capacity > 0, true),
      false
    ),
    registration_open
  )::event_registration_info
  from capacity;
$$;

GRANT ALL ON FUNCTION public.event_registration_info(e public.event) TO anonymous;
