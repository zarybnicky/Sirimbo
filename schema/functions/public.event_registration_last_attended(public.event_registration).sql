CREATE FUNCTION public.event_registration_last_attended(reg public.event_registration) RETURNS timestamp with time zone
    LANGUAGE sql STABLE
    AS $$
  select max(event_instance.since)
  from event_attendance
  join event_instance on event_instance.id = event_attendance.instance_id
  where event_attendance.registration_id = reg.id
    and event_attendance.status = 'attended'
$$;

GRANT ALL ON FUNCTION public.event_registration_last_attended(reg public.event_registration) TO anonymous;
