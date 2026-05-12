CREATE FUNCTION public.activity_timeline(p_since timestamp with time zone, p_until timestamp with time zone, p_person_ids bigint[] DEFAULT NULL::bigint[], p_cohort_id bigint DEFAULT NULL::bigint, p_kinds public.activity_timeline_kind[] DEFAULT NULL::public.activity_timeline_kind[], p_event_types public.event_type[] DEFAULT NULL::public.event_type[]) RETURNS SETOF public.activity_timeline_item
    LANGUAGE plpgsql STABLE
    AS $$
declare
  include_event_attendance boolean;
  include_competition_brief boolean;
  include_competition_result boolean;
begin
  if p_since is null or p_until is null or p_until <= p_since then
    return;
  end if;

  if p_person_ids is null and p_cohort_id is null then
    return;
  end if;

  if cardinality(p_kinds) = 0 then
    return;
  end if;

  include_event_attendance =
    (p_kinds is null or 'EVENT_ATTENDANCE'::public.activity_timeline_kind = any(p_kinds))
    and (p_event_types is null or cardinality(p_event_types) > 0);
  include_competition_brief =
    p_kinds is null or 'COMPETITION_BRIEF'::public.activity_timeline_kind = any(p_kinds);
  include_competition_result =
    p_kinds is null or 'COMPETITION_RESULT'::public.activity_timeline_kind = any(p_kinds);

  if include_event_attendance then
    return query
      with scoped_people as (
        select distinct p.id
        from public.person p
        where (p_person_ids is not null and p.id = any(p_person_ids))
           or (
             p_cohort_id is not null
             and exists (
               select 1
               from public.current_cohort_membership cm
               where cm.person_id = p.id
                 and cm.cohort_id = p_cohort_id
             )
           )
      )
      select
        ('event_attendance:' || ea.id)::text as id,
        'EVENT_ATTENDANCE'::public.activity_timeline_kind as kind,
        ei.since as sort_at,
        ei.since::date as activity_date,
        ea.person_id,
        p.name as person_name,
        ea.id as event_attendance_id,
        ei.id as event_instance_id,
        null::text as federation,
        null::text as federated_person_id,
        null::text as competitor_id,
        null::text as competitor_name,
        null::federated.competitor_type as competitor_type,
        null::bigint as competition_event_id,
        null::text as competition_event_name,
        null::text as competition_event_location,
        null::bigint as competition_id,
        null::date as competition_date,
        null::time as check_in_end,
        null::federated.category as category,
        null::text[] as dances,
        null::integer as participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        null::federated.competition_type as competition_type
      from public.event_attendance ea
      join public.event_instance ei on ei.id = ea.instance_id
      join public.person p on p.id = ea.person_id
      join scoped_people sp on sp.id = ea.person_id
      where ea.status <> 'cancelled'
        and ei.since >= p_since
        and ei.since < p_until
        and (p_event_types is null or ei.type = any(p_event_types));
  end if;

  if include_competition_result then
    return query
      select
        (
          'competition_result:' ||
          coalesce(cr.competition_id::text, '') || ':' ||
          coalesce(cr.competitor_id, '') || ':' ||
          coalesce(cr.person_id::text, '') || ':' ||
          coalesce((cr.category).id::text, '')
        )::text as id,
        'COMPETITION_RESULT'::public.activity_timeline_kind as kind,
        ((cr.competition_date + time '12:00')::timestamp)::timestamptz as sort_at,
        cr.competition_date as activity_date,
        cr.person_id,
        cr.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_instance_id,
        cr.federation,
        cr.federated_person_id,
        cr.competitor_id,
        cr.competitor_name,
        cr.competitor_type,
        cr.event_id as competition_event_id,
        cr.event_name as competition_event_name,
        cr.event_location as competition_event_location,
        cr.competition_id,
        cr.competition_date,
        null::time as check_in_end,
        cr.category,
        cr.dances,
        cr.participants,
        cr.ranking,
        cr.ranking_to,
        cr.point_gain,
        cr.is_final,
        cr.competition_type
      from (
        select *
        from public.competition_report(
          p_since::date,
          p_until::date,
          p_cohort_id,
          p_person_ids
        )
      ) as cr
      where cr.competition_date is not null
        and ((cr.competition_date + time '12:00')::timestamp)::timestamptz >= p_since
        and ((cr.competition_date + time '12:00')::timestamp)::timestamptz < p_until;
  end if;

  if include_competition_brief then
    return query
      with reports as (
        select
          cr.person_id,
          cr.competition_id,
          cr.competitor_id,
          (cr.category).id as category_id
        from (
          select *
          from public.competition_report(
            p_since::date,
            p_until::date,
            p_cohort_id,
            p_person_ids
          )
        ) as cr
        where include_competition_result
      )
      select
        (
          'competition_brief:' ||
          coalesce(cb.competition_id::text, '') || ':' ||
          coalesce(cb.competitor_id, '') || ':' ||
          coalesce(cb.person_id::text, '') || ':' ||
          coalesce((cb.category).id::text, '')
        )::text as id,
        'COMPETITION_BRIEF'::public.activity_timeline_kind as kind,
        ((cb.competition_date + coalesce(cb.check_in_end, time '12:00'))::timestamp)::timestamptz as sort_at,
        cb.competition_date as activity_date,
        cb.person_id,
        cb.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_instance_id,
        cb.federation,
        cb.federated_person_id,
        cb.competitor_id,
        cb.competitor_name,
        cb.competitor_type,
        cb.event_id as competition_event_id,
        cb.event_name as competition_event_name,
        cb.event_location as competition_event_location,
        cb.competition_id,
        cb.competition_date,
        cb.check_in_end,
        cb.category,
        cb.dances,
        cb.participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        cb.competition_type
      from (
        select *
        from public.competition_brief(
          p_since::date,
          p_until::date,
          p_cohort_id,
          p_person_ids
        )
      ) as cb
      where cb.competition_date is not null
        and ((cb.competition_date + coalesce(cb.check_in_end, time '12:00'))::timestamp)::timestamptz >= p_since
        and ((cb.competition_date + coalesce(cb.check_in_end, time '12:00'))::timestamp)::timestamptz < p_until
        and not exists (
          select 1
          from reports r
          where r.person_id is not distinct from cb.person_id
            and r.competition_id is not distinct from cb.competition_id
            and r.competitor_id is not distinct from cb.competitor_id
            and r.category_id is not distinct from (cb.category).id
        );
  end if;
end;
$$;

COMMENT ON FUNCTION public.activity_timeline(p_since timestamp with time zone, p_until timestamp with time zone, p_person_ids bigint[], p_cohort_id bigint, p_kinds public.activity_timeline_kind[], p_event_types public.event_type[]) IS '
@behavior +queryField:resource:list -queryField:resource:connection
';

GRANT ALL ON FUNCTION public.activity_timeline(p_since timestamp with time zone, p_until timestamp with time zone, p_person_ids bigint[], p_cohort_id bigint, p_kinds public.activity_timeline_kind[], p_event_types public.event_type[]) TO anonymous;
