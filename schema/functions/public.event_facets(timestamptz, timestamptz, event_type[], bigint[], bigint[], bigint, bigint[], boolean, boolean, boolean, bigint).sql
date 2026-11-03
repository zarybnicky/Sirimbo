CREATE FUNCTION public.event_facets(start_range timestamp with time zone, end_range timestamp with time zone, only_types public.event_type[] DEFAULT NULL::public.event_type[], trainer_ids bigint[] DEFAULT NULL::bigint[], participant_ids bigint[] DEFAULT NULL::bigint[], parent_id bigint DEFAULT NULL::bigint, location_ids bigint[] DEFAULT NULL::bigint[], can_register boolean DEFAULT NULL::boolean, only_mine boolean DEFAULT false, relevant_only boolean DEFAULT false, cohort_id bigint DEFAULT NULL::bigint) RETURNS TABLE(facet text, value text, count integer)
    LANGUAGE sql STABLE
    AS $_$
  with mine as (
    select event_id from event_registration
    where person_id = any((select current_person_ids())::bigint[])
      and registration_status = 'active'
    union all
    select event_id from event_trainer
    where person_id = any((select current_person_ids())::bigint[])
  ), cohort_people as (
    select cohort_id, person_id from current_cohort_membership
    where (not coalesce(only_mine, false)
        or person_id = any((select current_person_ids())::bigint[]))
      and (participant_ids is not null or coalesce(only_mine, false)
        or person_id in (
          select person_id from current_tenant_membership
          union all select person_id from current_tenant_trainer
          union all select person_id from current_tenant_administrator
        ))
  ), candidates as materialized (
    select i.id, i.type, i.location_id,
      array(select person_id from event_trainer where event_id = i.id) as trainers,
      array(select person_id from event_registration
        where event_id = i.id and registration_status = 'active') as participants
    from event i
    where i.tenant_id = current_tenant_id()
      and start_range is not null
      and app_private.tstzrange_overlaps(i.range, tstzrange(start_range, end_range, '[)'))
      and (can_register is null or (event_registration_info(i)).can_register = can_register)
      and ($6 is null or i.parent_id = $6)
      and (not coalesce(only_mine, false) or i.id in (select event_id from mine))
      and (not coalesce(relevant_only, false) or i.parent_id is null
        or i.id in (select event_id from mine)
        or i.parent_id in (select event_id from mine))
  ), matches as (
    select *,
      only_types is null or type = any(only_types) as type_matches,
      trainer_ids is null or trainers && trainer_ids as trainer_matches,
      participant_ids is null or case when $11 is null
        then participants && participant_ids
        else (trainers || participants) && array(
          select person_id from cohort_people c
          where c.cohort_id = $11 and person_id = any(participant_ids)
        ) end as participant_matches,
      $11 is null or (trainers || participants) && array(
        select person_id from cohort_people c where c.cohort_id = $11
      ) as cohort_matches,
      location_ids is null or location_id = any(location_ids) as location_matches
    from candidates
  ), facets as (
    select m.id, 'type' as facet, upper(m.type::text) as value
    from matches m
    where m.trainer_matches and m.participant_matches and m.location_matches and m.cohort_matches
    union all
    select m.id, 'trainer', unnest(m.trainers)::text
    from matches m
    where m.type_matches and m.participant_matches and m.location_matches and m.cohort_matches
    union all
    select m.id, 'participant', person_id::text
    from matches m
    cross join lateral unnest(m.participants) person(person_id)
    where m.type_matches and m.trainer_matches and m.location_matches and m.cohort_matches
      and ($11 is null or person_id in (
        select person_id from cohort_people c where c.cohort_id = $11
      ))
    union all
    select m.id, 'location', m.location_id::text
    from matches m
    where m.type_matches and m.trainer_matches and m.participant_matches and m.cohort_matches
      and m.location_id is not null
    union all
    select m.id, 'cohort', c.cohort_id::text
    from matches m
    cross join lateral unnest(m.trainers || m.participants) person(person_id)
    join cohort_people c using (person_id)
    where m.type_matches and m.trainer_matches and m.location_matches
      and (participant_ids is null or c.person_id = any(participant_ids))
  )
  select facet, value, count(*)::integer
  from (select distinct id, facet, value from facets) events
  group by facet, value;
$_$;

COMMENT ON FUNCTION public.event_facets(start_range timestamp with time zone, end_range timestamp with time zone, only_types public.event_type[], trainer_ids bigint[], participant_ids bigint[], parent_id bigint, location_ids bigint[], can_register boolean, only_mine boolean, relevant_only boolean, cohort_id bigint) IS '@behavior +list -connection';

GRANT ALL ON FUNCTION public.event_facets(start_range timestamp with time zone, end_range timestamp with time zone, only_types public.event_type[], trainer_ids bigint[], participant_ids bigint[], parent_id bigint, location_ids bigint[], can_register boolean, only_mine boolean, relevant_only boolean, cohort_id bigint) TO anonymous;
