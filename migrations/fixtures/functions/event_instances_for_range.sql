CREATE or replace FUNCTION public.event_instances_for_range(
  only_type public.event_type,
  start_range timestamp with time zone,
  end_range timestamp with time zone DEFAULT NULL::timestamp with time zone,
  trainer_ids bigint[] = null,
  only_mine boolean = false
) RETURNS SETOF event_instance as $$
  select i.*
  from event_instance i
  join event e on event_id=e.id and e.is_visible AND (only_type IS NULL OR e.type = only_type)
  where i.tenant_id = current_tenant_id()
    and i.range && tstzrange(start_range, coalesce(end_range, 'infinity'::timestamptz), '[]')
    and (trainer_ids is null
      or exists (select 1 from event_trainer where person_id = any (trainer_ids) and event_id = e.id)
      or exists (select 1 from event_instance_trainer where person_id = any (trainer_ids) and instance_id=i.id))
    and (only_mine is FALSE
      or i.event_id = any (
        select r.event_id from event_registration r where r.person_id = any ((select current_person_ids())::bigint[])
        union all
        select r.event_id from event_registration r where r.couple_id = any ((select current_couple_ids())::bigint[])
        union all
        SELECT et2.event_id FROM event_trainer et2 WHERE et2.person_id = ANY ((select current_person_ids())::bigint[])
      )
      OR i.id IN (
        SELECT eit2.instance_id FROM event_instance_trainer eit2 WHERE eit2.person_id = ANY ((select current_person_ids())::bigint[])));
$$ stable language sql;
COMMENT ON FUNCTION public.event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.event_instances_for_range TO anonymous;
