CREATE FUNCTION public.event_instance_target_cohorts(inst public.event_instance) RETURNS SETOF public.event_target_cohort
    LANGUAGE sql STABLE
    AS $$
  select * from event_target_cohort where event_id = inst.event_id;
$$;

COMMENT ON FUNCTION public.event_instance_target_cohorts(inst public.event_instance) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.event_instance_target_cohorts(inst public.event_instance) TO anonymous;
