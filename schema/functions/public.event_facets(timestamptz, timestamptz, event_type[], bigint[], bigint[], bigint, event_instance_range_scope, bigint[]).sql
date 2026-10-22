CREATE FUNCTION public.event_facets(start_range timestamp with time zone, end_range timestamp with time zone, only_types public.event_type[] DEFAULT NULL::public.event_type[], trainer_ids bigint[] DEFAULT NULL::bigint[], participant_ids bigint[] DEFAULT NULL::bigint[], parent_id bigint DEFAULT NULL::bigint, scope public.event_instance_range_scope DEFAULT 'all'::public.event_instance_range_scope, location_ids bigint[] DEFAULT NULL::bigint[]) RETURNS TABLE(facet text, value text, count integer)
    LANGUAGE sql STABLE
    AS $_$
  with candidates as (
    select i.id, i.type, i.location_id,
      array(select distinct person_id from event_trainer where event_id = i.id) as trainers,
      array(select distinct person_id from event_registration
        where event_id = i.id and registration_status = 'active') as participants
    from event_instances_for_range(null, start_range, end_range,
      parent_id => $6, scope => $7) i
  ), matches as (
    select *,
      only_types is null or type = any(only_types) as type_matches,
      trainer_ids is null or trainers && trainer_ids as trainer_matches,
      participant_ids is null or participants && participant_ids as participant_matches,
      location_ids is null or location_id = any(location_ids) as location_matches
    from candidates
  )
  select option.facet, option.value, count(distinct m.id)::integer
  from matches m
  cross join lateral (
    select 'type' as facet, upper(m.type::text) as value
    where m.trainer_matches and m.participant_matches and m.location_matches
    union all
    select 'trainer', unnest(m.trainers)::text
    where m.type_matches and m.participant_matches and m.location_matches
    union all
    select 'participant', unnest(m.participants)::text
    where m.type_matches and m.trainer_matches and m.location_matches
    union all
    select 'location', m.location_id::text
    where m.type_matches and m.trainer_matches and m.participant_matches and m.location_id is not null
  ) option
  group by option.facet, option.value
  order by option.facet, option.value;
$_$;

COMMENT ON FUNCTION public.event_facets(start_range timestamp with time zone, end_range timestamp with time zone, only_types public.event_type[], trainer_ids bigint[], participant_ids bigint[], parent_id bigint, scope public.event_instance_range_scope, location_ids bigint[]) IS '@behavior +list -connection';

REVOKE ALL ON FUNCTION public.event_facets(start_range timestamp with time zone, end_range timestamp with time zone, only_types public.event_type[], trainer_ids bigint[], participant_ids bigint[], parent_id bigint, scope public.event_instance_range_scope, location_ids bigint[]) FROM PUBLIC;
GRANT ALL ON FUNCTION public.event_facets(start_range timestamp with time zone, end_range timestamp with time zone, only_types public.event_type[], trainer_ids bigint[], participant_ids bigint[], parent_id bigint, scope public.event_instance_range_scope, location_ids bigint[]) TO anonymous;
