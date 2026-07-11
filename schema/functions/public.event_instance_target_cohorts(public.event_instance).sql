CREATE FUNCTION public.event_instance_target_cohorts(inst public.event_instance) RETURNS SETOF public.event_instance_target_cohort
    LANGUAGE sql STABLE
    AS $$
  select * from public.event_instance_target_cohort where instance_id = inst.id;
$$;

COMMENT ON FUNCTION public.event_instance_target_cohorts(inst public.event_instance) IS '@omit';

GRANT ALL ON FUNCTION public.event_instance_target_cohorts(inst public.event_instance) TO anonymous;
