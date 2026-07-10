CREATE FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, trainer_ids bigint[] DEFAULT NULL::bigint[], participant_ids bigint[] DEFAULT NULL::bigint[], only_mine boolean DEFAULT false, parent_id bigint DEFAULT NULL::bigint) RETURNS SETOF public.event_instance
    LANGUAGE sql STABLE
    AS $_$
  select i.*
  from event_instance i
  where i.tenant_id = current_tenant_id()
    and (only_type IS NULL OR i.type = only_type)
    and ($7 is null or $7 = i.parent_id)
    and i.since < coalesce(end_range, 'infinity'::timestamptz)
    and i.until > start_range
    and (trainer_ids is null
      or exists (select 1 from event_trainer where event_id = i.event_id and person_id = any (trainer_ids))
      or exists (select 1 from event_instance_trainer where instance_id = i.id and person_id = any (trainer_ids)))
    and (participant_ids is null
      or exists (select 1 from event_instance_registration where instance_id = i.id and person_id = any (participant_ids) and status <> 'cancelled'))
    and (only_mine is false
      or exists (select 1 from event_instance_registration where instance_id = i.id and person_id = any ((select current_person_ids())::bigint[]) and status <> 'cancelled')
      or exists (select 1 from event_trainer where event_id = i.event_id and person_id = any ((select current_person_ids())::bigint[]))
      or exists (select 1 from event_instance_trainer where instance_id = i.id and person_id = any ((select current_person_ids())::bigint[])));
$_$;

COMMENT ON FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, trainer_ids bigint[], participant_ids bigint[], only_mine boolean, parent_id bigint) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, trainer_ids bigint[], participant_ids bigint[], only_mine boolean, parent_id bigint) TO anonymous;
