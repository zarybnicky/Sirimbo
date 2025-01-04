CREATE or replace FUNCTION public.my_event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, only_mine boolean = false) RETURNS SETOF event_instance LANGUAGE sql STABLE
begin atomic
  select distinct on (instances.id) instances.*
  from event_instances_for_range(only_type, start_range, end_range) instances
  left join event_registration on event_registration.event_id=instances.event_id and (event_registration.person_id = any(current_person_ids()) or event_registration.couple_id = any(current_couple_ids()))
  left join event_trainer on event_trainer.event_id=instances.event_id and event_trainer.person_id = any(current_person_ids())
  left join event_instance_trainer on event_instance_trainer.instance_id=instances.id and event_instance_trainer.person_id = any(current_person_ids())
  where event_registration.id is not null or event_trainer.id is not null or event_instance_trainer.id is not null;
end;
COMMENT ON FUNCTION public.my_event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.my_event_instances_for_range TO anonymous;
