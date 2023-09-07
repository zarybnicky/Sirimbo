CREATE FUNCTION public.event_instances_for_range(only_mine boolean, only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone) RETURNS SETOF public.event_instance
    LANGUAGE sql STABLE
    AS $$
  select * from (
    select distinct on (event_instance.id) event_instance.*
    from event_instance
    join event on event_id=event.id
    left join event_registration on event_registration.event_id=event.id
    left join event_trainer on event_trainer.event_id=event.id
    left join event_instance_trainer on event_instance_trainer.instance_id=event_instance.id
    where only_mine
    and event.is_visible = true
    and tstzrange(start_range, end_range, '[]') && range
    and (only_type is null or event.type = only_type)
    and (
      event_registration.person_id in (select my_person_ids())
      or event_registration.couple_id in (select my_couple_ids())
      or event_trainer.person_id in (select my_person_ids())
      or event_instance_trainer.person_id in (select my_person_ids())
    )
    union
    select distinct on (event_instance.id) event_instance.*
    from event_instance
    join event on event_id=event.id
    where not only_mine
    and event.is_visible = true
    and tstzrange(start_range, end_range, '[]') && range
    and (only_type is null or event.type = only_type)
  ) a order by a.range;
$$;

COMMENT ON FUNCTION public.event_instances_for_range(only_mine boolean, only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.event_instances_for_range(only_mine boolean, only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone) TO anonymous;


