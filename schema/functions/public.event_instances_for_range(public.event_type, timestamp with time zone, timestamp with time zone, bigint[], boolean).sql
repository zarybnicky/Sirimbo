CREATE FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, trainer_ids bigint[] DEFAULT NULL::bigint[], only_mine boolean DEFAULT false) RETURNS SETOF public.event_instance
    LANGUAGE sql STABLE
    AS $$
  select i.*
  from event_instance i
  join event on event_id=event.id
  where i.since <= COALESCE(end_range, 'infinity'::timestamptz)
    and i.until >= start_range
    AND i.event_id IN (SELECT e.id FROM public.event e WHERE e.is_visible AND (only_type IS NULL OR e.type = only_type))
    and (trainer_ids is null
      or exists (select 1 from event_trainer where person_id = any (trainer_ids) and event_id = event.id)
      or exists (select 1 from event_instance_trainer where person_id = any (trainer_ids) and instance_id=i.id))
    and (only_mine is FALSE
      or i.event_id IN (
        SELECT r.event_id FROM event_registration r WHERE r.person_id = ANY (current_person_ids()) OR r.couple_id = ANY (current_couple_ids()))
      OR i.event_id IN (
        SELECT et2.event_id FROM event_trainer et2 WHERE et2.person_id = ANY (current_person_ids()))
      OR i.id IN (
        SELECT eit2.instance_id FROM event_instance_trainer eit2 WHERE eit2.person_id = ANY (current_person_ids())));
$$;

COMMENT ON FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, trainer_ids bigint[], only_mine boolean) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, trainer_ids bigint[], only_mine boolean) TO anonymous;
