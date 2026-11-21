create or replace function event_conflicts(
  start_range timestamptz,
  end_range timestamptz,
  condition event_search_condition default null
) returns setof event_conflict
language sql stable as $$
  with events as materialized (
    select * from event e
    where e.tenant_id = current_tenant_id() and not e.is_cancelled
      and start_range is not null
      and app_private.tstzrange_overlaps(e.range, tstzrange(start_range, end_range, '[)'))
  ), participation as materialized (
    select e.id as event_id, p.*
    from events e
    cross join lateral (
      select person_id, true as trainer, null::bigint as couple_id
      from event_trainer where event_id = e.id
      union all
      select r.person_id, false, parent.couple_id
      from event_registration r
      left join event_registration parent on parent.id = r.parent_registration_id
      where r.event_id = e.id and r.registration_status = 'active' and r.person_id is not null
    ) p
  ), mine as (
    select event_id from event_registration
    where person_id = any((select current_person_ids())::bigint[]) and registration_status = 'active'
    union all
    select event_id from event_trainer
    where person_id = any((select current_person_ids())::bigint[])
  ), selected as (
    select e.id from events e
    where ((condition).types is null or e.type = any((condition).types))
      and ((condition).location_ids is null or e.location_id = any((condition).location_ids))
      and ((condition).parent_id is null or e.parent_id = (condition).parent_id)
      and ((condition).can_register is null or (event_registration_info(e::event)).can_register = (condition).can_register)
      and (not coalesce((condition).only_mine, false) or e.id in (select event_id from mine))
      and (not coalesce((condition).relevant_only, false) or e.parent_id is null
        or e.id in (select event_id from mine) or e.parent_id in (select event_id from mine))
      and ((condition).trainer_ids is null or exists (
        select 1 from participation p where p.event_id = e.id and p.trainer
          and p.person_id = any((condition).trainer_ids)
      ))
      and ((condition).couple_id is null or exists (
        select 1 from event_registration r where r.event_id = e.id
          and r.registration_status = 'active' and r.couple_id = (condition).couple_id
      ))
      and (((condition).participant_ids is null and (condition).cohort_id is null) or exists (
        select 1 from participation p
        where p.event_id = e.id
          and ((condition).couple_id is null or p.couple_id = (condition).couple_id)
          and ((condition).participant_ids is null or p.person_id = any((condition).participant_ids))
          and ((condition).cohort_id is null or exists (
            select 1 from current_cohort_membership c
            where c.cohort_id = (condition).cohort_id and c.person_id = p.person_id
          ))
      ))
  ), instances as materialized (
    select e.id, e.name, e.since, e.until, e.range, e.parent_id, p.person_id
    from events e join (select distinct event_id, person_id from participation) p on p.event_id = e.id
  )
  select a.person_id, p.name,
    a.id, a.name, a.since, a.until,
    b.id, b.name, b.since, b.until,
    tstzrange(greatest(a.since, b.since), least(a.until, b.until), '[)')
  from instances a
  join instances b on a.person_id = b.person_id and a.id < b.id
    and a.parent_id is distinct from b.id and b.parent_id is distinct from a.id
    and app_private.tstzrange_overlaps(a.range, b.range)
    and greatest(a.since, b.since) < least(a.until, b.until)
  join person p on p.id = a.person_id
  -- Keep the counterpart even when only one event matches the calendar selectors.
  where a.id in (select id from selected) or b.id in (select id from selected)
  order by a.since, a.id, b.since, b.id, a.person_id;
$$;
comment on function event_conflicts is '@behavior +list -connection';
grant execute on function event_conflicts to anonymous;
