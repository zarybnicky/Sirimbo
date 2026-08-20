CREATE FUNCTION public.event_instance_registration_info(inst public.event_instance) RETURNS public.event_instance_registration_info
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'public', 'pg_catalog', 'pg_temp'
    AS $$
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

GRANT ALL ON FUNCTION public.event_instance_registration_info(inst public.event_instance) TO anonymous;
