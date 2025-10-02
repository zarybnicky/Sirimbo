create or replace function public.event_registration_last_attended(reg public.event_registration)
returns timestamp with time zone
language sql
stable
as $$
  select max(event_instance.since)
  from public.event_attendance
  join public.event_instance on event_instance.id = event_attendance.instance_id
  where event_attendance.registration_id = reg.id
    and event_attendance.status = 'ATTENDED'
$$;

grant all on function public.event_registration_last_attended(public.event_registration) to anonymous;
