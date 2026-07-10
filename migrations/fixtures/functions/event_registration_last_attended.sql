create or replace function public.event_registration_last_attended(reg public.event_registration)
  returns timestamp with time zone
  language sql stable as $$
  select max(event_instance.since)
  from public.event_instance_registration eir
  join public.event_instance on event_instance.id = eir.instance_id
  where eir.legacy_registration_id = reg.id
    and eir.person_id is not null
    and eir.status = 'attended'
$$;

grant all on function public.event_registration_last_attended(public.event_registration) to anonymous;
