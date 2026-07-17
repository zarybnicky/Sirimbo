CREATE FUNCTION public.event_instance_registration_last_attended(reg public.event_instance_registration) RETURNS timestamp with time zone
    LANGUAGE sql STABLE
    AS $$
  select max(attended_instance.since)
  from event_instance_registration r
  join event_instance attended_instance on attended_instance.id = r.instance_id
  join event_instance current_instance on current_instance.id = reg.instance_id
  where current_instance.series_id is not null
    and attended_instance.tenant_id = current_instance.tenant_id
    and attended_instance.series_id = current_instance.series_id
    and r.person_id = reg.person_id
    and r.status = 'attended'
$$;

GRANT ALL ON FUNCTION public.event_instance_registration_last_attended(reg public.event_instance_registration) TO anonymous;
