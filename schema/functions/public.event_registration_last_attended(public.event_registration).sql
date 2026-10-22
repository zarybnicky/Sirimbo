CREATE FUNCTION public.event_registration_last_attended(reg public.event_registration) RETURNS timestamp with time zone
    LANGUAGE sql STABLE
    AS $$
  select max(attended.since)
  from event_registration r
  join event current on current.id = reg.event_id
  join event attended on attended.id = r.event_id
  where current.series_id is not null
    and attended.series_id = current.series_id
    and r.person_id = reg.person_id
    and r.status = 'attended'
$$;

GRANT ALL ON FUNCTION public.event_registration_last_attended(reg public.event_registration) TO anonymous;
