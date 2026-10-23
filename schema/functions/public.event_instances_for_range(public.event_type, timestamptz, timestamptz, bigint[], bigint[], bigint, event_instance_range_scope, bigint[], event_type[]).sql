CREATE FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, trainer_ids bigint[] DEFAULT NULL::bigint[], participant_ids bigint[] DEFAULT NULL::bigint[], parent_id bigint DEFAULT NULL::bigint, scope public.event_instance_range_scope DEFAULT 'all'::public.event_instance_range_scope, location_ids bigint[] DEFAULT NULL::bigint[], only_types public.event_type[] DEFAULT NULL::public.event_type[]) RETURNS SETOF public.event
    LANGUAGE sql STABLE
    AS $_$
  with mine as (
    select event_id from event_registration
    where person_id = any ((select current_person_ids())::bigint[]) and registration_status = 'active'
    union all
    select event_id from event_trainer
    where person_id = any ((select current_person_ids())::bigint[])
  )
  select i.*
  from event i
  where i.tenant_id = current_tenant_id()
    and (only_type is null or i.type = only_type)
    and (only_types is null or i.type = any(only_types))
    and case
      when $6 is not null then i.parent_id = $6
        and (scope <> 'mine' or i.id in (select event_id from mine))
      when scope = 'all' then true
      when scope = 'top_level' then i.parent_id is null
      when scope = 'mine' then i.id in (select event_id from mine)
      when scope = 'relevant' then i.parent_id is null
        or i.id in (select event_id from mine)
        or i.parent_id in (select event_id from mine)
    end
    and start_range is not null
    and i.since < coalesce(end_range, 'infinity'::timestamptz)
    and app_private.tstzrange_overlaps(i.range, tstzrange(
      start_range, greatest(start_range, coalesce(end_range, 'infinity'::timestamptz)), '[]'))
    and (trainer_ids is null
      or exists (select 1 from event_trainer where event_id = i.id and person_id = any (trainer_ids)))
    and (participant_ids is null
      or exists (select 1 from event_registration where event_id = i.id and person_id = any (participant_ids) and registration_status = 'active'))
    and (location_ids is null or i.location_id = any (location_ids))
  order by i.since
  ;
$_$;

COMMENT ON FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, trainer_ids bigint[], participant_ids bigint[], parent_id bigint, scope public.event_instance_range_scope, location_ids bigint[], only_types public.event_type[]) IS '@behavior +list -connection';

GRANT ALL ON FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone, trainer_ids bigint[], participant_ids bigint[], parent_id bigint, scope public.event_instance_range_scope, location_ids bigint[], only_types public.event_type[]) TO anonymous;
