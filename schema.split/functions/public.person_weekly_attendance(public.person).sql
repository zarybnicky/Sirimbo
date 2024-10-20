CREATE FUNCTION public.person_weekly_attendance(p public.person) RETURNS TABLE(week date, event_count integer)
    LANGUAGE sql STABLE
    AS $$
  select date_trunc('week', since) as week, count(*) as count
  from event_attendance
  join event_instance on instance_id=event_instance.id
  where person_id = p.id and since <= (CURRENT_DATE + interval '1 day')
  group by date_trunc('week', since)
$$;

COMMENT ON FUNCTION public.person_weekly_attendance(p public.person) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.person_weekly_attendance(p public.person) TO anonymous;
