--! Previous: sha1:c4881b00f85b442248ec09d7be2ab03b0cae5c2e
--! Hash: sha1:74faa7fd530166ad039377826ac50620f1d49fa6

--! split: 1-current.sql
CREATE or replace FUNCTION app_private.event_registration_person_ids(e public.event_registration) RETURNS SETOF bigint
    LANGUAGE sql security definer
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select e.person_id as id where e.person_id is not null
  union
  select unnest(array[man_id, woman_id]) as id from couple where couple.id = e.couple_id and e.couple_id is not null
$$;
grant all on function app_private.event_registration_person_ids to anonymous;

CREATE or replace FUNCTION app_private.refresh_event_instance_stats(p_instance_id bigint) RETURNS void
    LANGUAGE sql security definer
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  with v as (
    select jsonb_build_object(
      'TOTAL', agg.person_total,
      'UNKNOWN', agg.unknown_count,
      'ATTENDED', agg.attended_count,
      'NOT_EXCUSED', agg.not_excused_count
    ) as stats
    from (
      select
        count(*) filter (where ea.status <> 'cancelled')::int as person_total,
        count(*) filter (where ea.status = 'unknown')::int as unknown_count,
        count(*) filter (where ea.status = 'attended')::int as attended_count,
        count(*) filter (where ea.status = 'not-excused')::int as not_excused_count
      from public.event_attendance ea
      where ea.instance_id = p_instance_id
    ) agg
  )
  update public.event_instance ei set stats = v.stats from v
  where ei.id = p_instance_id and ei.stats is distinct from v.stats;
$$;
grant all on function app_private.refresh_event_instance_stats to anonymous;

CREATE or replace FUNCTION app_private.refresh_event_instance_manager_person_ids(p_instance_id bigint, p_event_id bigint) RETURNS boolean
    LANGUAGE sql security definer
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  with v as (
    select app_private.event_instance_manager_person_ids(p_instance_id, p_event_id) as ids
  ),
  u as (
    update public.event_instance ei set manager_person_ids = v.ids from v
    where ei.id = p_instance_id and ei.event_id = p_event_id and ei.manager_person_ids is distinct from v.ids
    returning 1
  )
  select exists(select 1 from u);
$$;
grant all on function app_private.refresh_event_instance_manager_person_ids to anonymous;

DROP FUNCTION IF EXISTS public.event_instance_attendance_summary;

DROP FUNCTION IF EXISTS event_instances_for_range(only_type event_type, start_range timestamp with time zone, end_range timestamp with time zone, trainer_ids bigint[], only_mine boolean);
--! Included functions/event_instances_for_range.sql
CREATE or replace FUNCTION public.event_instances_for_range(
  only_type public.event_type,
  start_range timestamp with time zone,
  end_range timestamp with time zone DEFAULT NULL::timestamp with time zone,
  trainer_ids bigint[] = null,
  participant_ids bigint[] = null,
  only_mine boolean = false
) RETURNS SETOF event_instance as $$
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
$$ stable language sql;

COMMENT ON FUNCTION public.event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.event_instances_for_range TO anonymous;
--! EndIncluded functions/event_instances_for_range.sql
