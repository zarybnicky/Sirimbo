create or replace function public.event_instance_registration_last_attended(reg public.event_instance_registration)
  returns timestamp with time zone
  language sql stable as $$
  select max(ei.since)
  from public.event_instance_registration r
  join public.event_instance ei on ei.id = r.instance_id
  where reg.legacy_registration_id is not null
    and r.legacy_registration_id = reg.legacy_registration_id
    and r.person_id is not null
    and r.status = 'attended'
$$;

grant all on function public.event_instance_registration_last_attended(public.event_instance_registration) to anonymous;
