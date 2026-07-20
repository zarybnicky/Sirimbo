create or replace function event_instance_registration_last_attended(reg event_instance_registration)
  returns timestamptz
  language sql stable
as $$
  select max(attended.since)
  from event_instance_registration r
  join event_instance current on current.id = reg.instance_id
  join event_instance attended on attended.id = r.instance_id
  where current.series_id is not null
    and attended.series_id = current.series_id
    and r.person_id = reg.person_id
    and r.status = 'attended'
$$;

grant all on function event_instance_registration_last_attended to anonymous;
