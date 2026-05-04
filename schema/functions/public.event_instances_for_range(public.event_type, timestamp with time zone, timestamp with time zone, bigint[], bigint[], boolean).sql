CREATE FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, trainer_ids bigint[] DEFAULT NULL::bigint[], participant_ids bigint[] DEFAULT NULL::bigint[], only_mine boolean DEFAULT false) RETURNS SETOF public.event_instance
    LANGUAGE sql STABLE
    AS $$
  select i.*
  from event_instance i
  where i.tenant_id = current_tenant_id()
    and (only_type IS NULL OR i.type = only_type)
    and i.since < coalesce(end_range, 'infinity'::timestamptz)
    and i.until > start_range
    and (trainer_ids is null
      or exists (select 1 from event_trainer where event_id = i.event_id and person_id = any (trainer_ids))
      or exists (select 1 from event_instance_trainer where instance_id = i.id and person_id = any (trainer_ids)))
    and (participant_ids is null
      or exists (select 1 from event_attendance a where a.instance_id = i.id and a.person_id = any (participant_ids) and a.status <> 'cancelled'))
    and (only_mine is false
      or exists (select 1 from event_attendance where instance_id = i.id and person_id = any ((select current_person_ids())::bigint[]) and status <> 'cancelled')
      or exists (select 1 from event_trainer where event_id = i.event_id and person_id = any ((select current_person_ids())::bigint[]))
      or exists (select 1 from event_instance_trainer where instance_id = i.id and person_id = any ((select current_person_ids())::bigint[])));
$$;

COMMENT ON FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, trainer_ids bigint[], participant_ids bigint[], only_mine boolean) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, trainer_ids bigint[], participant_ids bigint[], only_mine boolean) TO anonymous;
