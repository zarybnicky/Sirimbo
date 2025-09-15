CREATE or replace FUNCTION event_instance_attendance_summary(e event_instance)
    RETURNS TABLE(status attendance_type, count integer)
    LANGUAGE sql STABLE
    AS $$
  select status, count(status) as count from event_attendance where instance_id=e.id group by status;
$$;

COMMENT ON FUNCTION event_instance_attendance_summary(e event_instance) IS '@simpleCollections only';

GRANT ALL ON FUNCTION event_instance_attendance_summary(e event_instance) TO anonymous;
