CREATE FUNCTION public.event_instance_attendance_summary(e public.event_instance) RETURNS TABLE(status public.attendance_type, count integer)
    LANGUAGE sql STABLE
    AS $$
  select status, count(status) as count from event_attendance where instance_id=e.id group by status;
$$;

COMMENT ON FUNCTION public.event_instance_attendance_summary(e public.event_instance) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.event_instance_attendance_summary(e public.event_instance) TO anonymous;


