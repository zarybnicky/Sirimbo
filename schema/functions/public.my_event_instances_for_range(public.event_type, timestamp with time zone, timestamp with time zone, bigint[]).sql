CREATE FUNCTION public.my_event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, trainer_ids bigint[] DEFAULT NULL::bigint[]) RETURNS SETOF public.event_instance
    LANGUAGE sql STABLE
    AS $$
  select i.*
  from event_instance i
  join event on event_id=event.id
  where event.is_visible
    and i.since <= end_range
    and i.until >= start_range
    and (only_type is null or event.type = only_type)
    and (trainer_ids is null
      OR i.event_id IN (SELECT et.event_id FROM event_trainer et WHERE et.person_id = ANY (trainer_ids))
      OR i.id IN (SELECT eit.instance_id FROM event_instance_trainer eit WHERE eit.person_id = ANY (trainer_ids)))
    and (
      i.event_id IN (
        SELECT r.event_id FROM event_registration r WHERE r.person_id = ANY (current_person_ids()) OR r.couple_id = ANY (current_couple_ids())
      ) OR i.event_id IN (
        SELECT et2.event_id FROM event_trainer et2 WHERE et2.person_id = ANY (current_person_ids())
      ) OR i.id IN (
        SELECT eit2.instance_id FROM event_instance_trainer eit2 WHERE eit2.person_id = ANY (current_person_ids())
      )
    );
$$;

COMMENT ON FUNCTION public.my_event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, trainer_ids bigint[]) IS '@deprecated
@simpleCollections only';

GRANT ALL ON FUNCTION public.my_event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, trainer_ids bigint[]) TO anonymous;
