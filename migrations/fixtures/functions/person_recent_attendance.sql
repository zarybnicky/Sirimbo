CREATE or replace FUNCTION public.person_recent_attendance(p public.person) RETURNS SETOF public.event_attendance
    LANGUAGE sql STABLE
    AS $$
  select event_attendance.*
  from event_attendance
  join event_instance on instance_id=event_instance.id
  where person_id = p.id
    and since <= (CURRENT_DATE + interval '1 day')
    and since > (CURRENT_DATE - interval '6 month')
  order by since desc
$$;

COMMENT ON FUNCTION public.person_recent_attendance(p public.person) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.person_recent_attendance(p public.person) TO anonymous;
