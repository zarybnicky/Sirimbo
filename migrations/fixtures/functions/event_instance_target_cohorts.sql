drop function if exists public.event_instance_target_cohorts(public.event_instance);

create function public.event_instance_target_cohorts(inst public.event_instance)
  returns setof public.event_instance_target_cohort
  language sql stable as $$
  select * from public.event_instance_target_cohort where instance_id = inst.id;
$$;

comment on function public.event_instance_target_cohorts(public.event_instance)
  is '@omit';
grant all on function public.event_instance_target_cohorts(public.event_instance) to anonymous;
