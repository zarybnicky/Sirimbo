CREATE or replace FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, only_mine boolean = false, trainer_ids bigint[] = null) RETURNS SETOF event_instance LANGUAGE sql STABLE
begin atomic
  select event_instance.*
  from event_instance
  join event on event_id=event.id
  where event.is_visible
    and event_instance.since <= end_range
    and event_instance.until >= start_range
    and (only_type is null or event.type = only_type)
    and (trainer_ids is null
      or exists (select 1 from event_trainer where person_id = any (trainer_ids) and event_id = event.id)
      or exists (select 1 from event_instance_trainer where person_id = any (trainer_ids) and instance_id=event_instance.id));
end;
COMMENT ON FUNCTION public.event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.event_instances_for_range TO anonymous;
