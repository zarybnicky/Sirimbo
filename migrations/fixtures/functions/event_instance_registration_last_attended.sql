create or replace function public.event_instance_registration_last_attended(reg public.event_instance_registration)
  returns timestamp with time zone
  language sql stable as $$
  select max(attended_instance.since)
  from public.event_instance_registration r
  join public.event_instance attended_instance on attended_instance.id = r.instance_id
  join public.event_instance current_instance on current_instance.id = reg.instance_id
  where current_instance.series_id is not null
    and attended_instance.tenant_id = current_instance.tenant_id
    and attended_instance.series_id = current_instance.series_id
    and r.person_id = reg.person_id
    and r.status = 'attended'
$$;

grant all on function public.event_instance_registration_last_attended(public.event_instance_registration) to anonymous;
