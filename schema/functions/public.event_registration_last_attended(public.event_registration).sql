CREATE FUNCTION public.event_registration_last_attended(reg public.event_registration) RETURNS timestamp with time zone
    LANGUAGE sql STABLE
    AS $$
  select max(event_instance.since)
  from public.event_instance_registration eir
  join public.event_instance on event_instance.id = eir.instance_id
  where eir.legacy_registration_id = reg.id
    and eir.person_id is not null
    and eir.status = 'attended'
$$;

GRANT ALL ON FUNCTION public.event_registration_last_attended(reg public.event_registration) TO anonymous;
