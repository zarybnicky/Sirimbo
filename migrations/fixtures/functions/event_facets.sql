drop function if exists event_instances_for_range;
drop function if exists event_facets;
drop type if exists event_instance_range_scope;

create or replace function event_facets(
  start_range timestamptz,
  end_range timestamptz,
  condition event_search_condition default null
) returns table(facet text, value text, count integer)
language sql stable as $$
  with mine as (
    select event_id from event_registration
    where person_id = any((select current_person_ids())::bigint[])
      and registration_status = 'active'
    union all
    select event_id from event_trainer
    where person_id = any((select current_person_ids())::bigint[])
  ), cohort_people as (
    select cohort_id, person_id from current_cohort_membership
  ), candidates as materialized (
    select i.id, i.type, i.location_id,
      array(select person_id from event_trainer where event_id = i.id) as trainers,
      array(select r.person_id from event_registration r
        left join event_registration parent on parent.id = r.parent_registration_id
        where r.event_id = i.id and r.registration_status = 'active' and r.person_id is not null
          and ((condition).couple_id is null or parent.couple_id = (condition).couple_id)) as participants
    from event i
    where i.tenant_id = current_tenant_id()
      and start_range is not null
      and app_private.tstzrange_overlaps(i.range, tstzrange(start_range, end_range, '[)'))
      and ((condition).can_register is null or (event_registration_info(i)).can_register = (condition).can_register)
      and ((condition).parent_id is null or i.parent_id = (condition).parent_id)
      and ((condition).couple_id is null or exists (
        select 1 from event_registration r where r.event_id = i.id
          and r.registration_status = 'active' and r.couple_id = (condition).couple_id
      ))
      and (not coalesce((condition).only_mine, false) or i.id in (select event_id from mine))
      and (not coalesce((condition).relevant_only, false) or i.parent_id is null
        or i.id in (select event_id from mine)
        or i.parent_id in (select event_id from mine))
  ), event_people as (
    select *, case when (condition).couple_id is null then trainers || participants
      else participants end as selected_people
    from candidates
  ), matches as (
    select *,
      (condition).types is null or type = any((condition).types) as type_matches,
      (condition).trainer_ids is null or trainers && (condition).trainer_ids as trainer_matches,
      (condition).participant_ids is null or case when (condition).cohort_id is null
        then selected_people && (condition).participant_ids
        else selected_people && array(
          select person_id from cohort_people c
          where c.cohort_id = (condition).cohort_id and person_id = any((condition).participant_ids)
        ) end as participant_matches,
      (condition).cohort_id is null or selected_people && array(
        select person_id from cohort_people c where c.cohort_id = (condition).cohort_id
      ) as cohort_matches,
      (condition).location_ids is null or location_id = any((condition).location_ids) as location_matches
    from event_people
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
    cross join lateral unnest(m.selected_people) person(person_id)
    where m.type_matches and m.trainer_matches and m.location_matches and m.cohort_matches
      and ((condition).cohort_id is null or person_id in (
        select person_id from cohort_people c where c.cohort_id = (condition).cohort_id
      ))
    union all
    select m.id, 'location', m.location_id::text
    from matches m
    where m.type_matches and m.trainer_matches and m.participant_matches and m.cohort_matches
      and m.location_id is not null
    union all
    select m.id, 'cohort', c.cohort_id::text
    from matches m
    cross join lateral unnest(m.selected_people) person(person_id)
    join cohort_people c using (person_id)
    where m.type_matches and m.trainer_matches and m.location_matches
      and ((condition).participant_ids is null or c.person_id = any((condition).participant_ids))
  )
  select facet, value, count(*)::integer
  from (select distinct id, facet, value from facets) events
  group by facet, value;
$$;
comment on function event_facets is '@behavior +list -connection';
grant execute on function event_facets to anonymous;
