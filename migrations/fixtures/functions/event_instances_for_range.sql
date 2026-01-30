CREATE or replace FUNCTION public.event_instances_for_range(
  only_type public.event_type,
  start_range timestamp with time zone,
  end_range timestamp with time zone DEFAULT NULL::timestamp with time zone,
  trainer_ids bigint[] = null,
  only_mine boolean = false
) RETURNS SETOF event_instance as $$
  select i.*
  from event_instance i
  where i.tenant_id = current_tenant_id()
    and (only_type IS NULL OR i.type = only_type)
    and i.since < coalesce(end_range, 'infinity'::timestamptz)
    and i.until > start_range
    and (trainer_ids is null
      or exists (select 1 from event_trainer where person_id = any (trainer_ids) and event_id = i.event_id)
      or exists (select 1 from event_instance_trainer where person_id = any (trainer_ids) and instance_id = i.id))
    and (only_mine is FALSE
      or i.event_id in (select r.event_id from event_registration r where r.person_id = any ((select current_person_ids())::bigint[]))
      or i.event_id in (select r.event_id from event_registration r where r.couple_id = any ((select current_couple_ids())::bigint[]))
      or i.event_id in (SELECT et.event_id FROM event_trainer et WHERE et.person_id = ANY ((select current_person_ids())::bigint[]))
      OR i.id IN (SELECT eit.instance_id FROM event_instance_trainer eit WHERE eit.person_id = ANY ((select current_person_ids())::bigint[])));
$$ stable language sql;
COMMENT ON FUNCTION public.event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.event_instances_for_range TO anonymous;
