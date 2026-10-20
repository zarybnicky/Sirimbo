create or replace function event_facets(
  start_range timestamptz,
  end_range timestamptz,
  only_types event_type[] default null,
  trainer_ids bigint[] default null,
  participant_ids bigint[] default null,
  parent_id bigint default null,
  scope event_instance_range_scope default 'all',
  location_ids bigint[] default null
) returns table(facet text, value text, count integer)
language sql stable as $$
  with candidates as (
    select i.id, i.type, i.location_id,
      array(select distinct person_id from event_instance_trainer where instance_id = i.id) as trainers,
      array(select distinct person_id from event_instance_registration
        where instance_id = i.id and registration_status = 'active') as participants
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
$$;
comment on function event_facets is '@behavior +list -connection';
revoke all on function event_facets from public;
grant execute on function event_facets to anonymous;
