create or replace view public.activity_timeline_item as
select
  null::text as id,
  null::public.activity_timeline_kind as kind,
  null::timestamptz as sort_at,
  null::date as activity_date,
  null::bigint as person_id,
  null::text as person_name,
  null::bigint as event_attendance_id,
  null::bigint as event_instance_id,
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
  null::federated.competition_type as competition_type,
  null::text as competition_event_external_id,
  null::text as competition_external_id
where false;

comment on view public.activity_timeline_item is $$
@primaryKey id
@interface mode:single type:kind
@type EVENT_ATTENDANCE name:ActivityEventAttendance attributes:event_attendance_id,event_instance_id
@type COMPETITION_BRIEF name:ActivityCompetitionBrief attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,check_in_end,category,dances,participants,competition_type,competition_event_external_id,competition_external_id
@type COMPETITION_RESULT name:ActivityCompetitionResult attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,category,dances,participants,ranking,ranking_to,point_gain,is_final,competition_type,competition_event_external_id,competition_external_id
@type JUDGING name:ActivityJudging attributes:federation,federated_person_id,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,category,competition_type,competition_event_external_id,competition_external_id
@type BIRTHDAY name:ActivityBirthday
@foreignKey (person_id) references person (id)|@fieldName person|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_attendance_id) references event_instance_registration (id)|@fieldName eventAttendance|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_instance_id) references event_instance (id)|@fieldName eventInstance|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@behavior -query:resource:list -query:resource:connection -query:resource:single
$$;

grant select on public.activity_timeline_item to anonymous;

CREATE OR REPLACE FUNCTION public.activity_timeline(p_since timestamp with time zone, p_until timestamp with time zone, p_person_ids bigint[] DEFAULT NULL::bigint[], p_cohort_id bigint DEFAULT NULL::bigint, p_kinds activity_timeline_kind[] DEFAULT NULL::activity_timeline_kind[], p_event_types event_type[] DEFAULT NULL::event_type[])
 RETURNS SETOF activity_timeline_item
 LANGUAGE plpgsql
 STABLE
AS $function$
declare
  include_event_attendance boolean;
  include_competition_brief boolean;
  include_competition_result boolean;
  include_judging boolean;
  include_birthday boolean;
begin
  if p_since is null or p_until is null or p_until <= p_since then
    return;
  end if;

  if cardinality(p_kinds) = 0 then
    return;
  end if;

  include_event_attendance =
    (p_kinds is null or 'EVENT_ATTENDANCE'::activity_timeline_kind = any(p_kinds))
    and (p_event_types is null or cardinality(p_event_types) > 0);
  include_competition_brief =
    p_kinds is null or 'COMPETITION_BRIEF'::activity_timeline_kind = any(p_kinds);
  include_competition_result =
    p_kinds is null or 'COMPETITION_RESULT'::activity_timeline_kind = any(p_kinds);
  include_judging =
    p_kinds is null or 'JUDGING'::activity_timeline_kind = any(p_kinds);
  include_birthday =
    p_kinds is null or 'BIRTHDAY'::activity_timeline_kind = any(p_kinds);

  if include_event_attendance then
    return query
      with scoped_people as (
        select distinct p.id, p.name
        from person p
        where (exists (select 1 from current_tenant_membership where person_id = p.id)
           or exists (select 1 from current_tenant_trainer where person_id = p.id)
           or exists (select 1 from current_tenant_administrator where person_id = p.id))
          and (p_person_ids is null or p.id = any(p_person_ids))
          and (p_cohort_id is null or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
      )
      select
        ('event_attendance:' || ea.id)::text as id,
        'EVENT_ATTENDANCE'::activity_timeline_kind as kind,
        ei.since as sort_at,
        ei.since::date as activity_date,
        ea.person_id,
        sp.name as person_name,
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
        null::federated.competition_type as competition_type,
        null::text as competition_event_external_id,
        null::text as competition_external_id
      from event_instance_registration ea
      join event_instance ei on ei.id = ea.instance_id
      join scoped_people sp on sp.id = ea.person_id
      where ea.registration_status = 'active'
        and ea.status <> 'cancelled'
        and ei.since >= p_since
        and ei.since < p_until
        and (p_event_types is null or ei.type = any(p_event_types));
  end if;

  if include_birthday then
    return query
      with scoped_people as (
        select distinct p.id, p.name, p.birth_date
        from person p
        where (exists (select 1 from current_tenant_membership where person_id = p.id)
           or exists (select 1 from current_tenant_trainer where person_id = p.id)
           or exists (select 1 from current_tenant_administrator where person_id = p.id))
          and (p_person_ids is null or p.id = any(p_person_ids))
          and (p_cohort_id is null or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
          and p.birth_date is not null
      ),
      birthdays as (
        select
          sp.id as person_id,
          sp.name as person_name,
          make_date(
            years.year,
            extract(month from sp.birth_date)::int,
            least(
              extract(day from sp.birth_date)::int,
              extract(day from (
                make_date(years.year, extract(month from sp.birth_date)::int, 1)
                + interval '1 month - 1 day'
              ))::int
            )
          ) as birthday_date
        from scoped_people sp
        cross join generate_series(extract(year from p_since)::int, extract(year from p_until)::int) as years(year)
      )
      select
        ('birthday:' || b.person_id || ':' || b.birthday_date)::text as id,
        'BIRTHDAY'::activity_timeline_kind as kind,
        ((b.birthday_date + time '12:00')::timestamp)::timestamptz as sort_at,
        b.birthday_date as activity_date,
        b.person_id,
        b.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_instance_id,
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
        null::federated.competition_type as competition_type,
        null::text as competition_event_external_id,
        null::text as competition_external_id
      from birthdays b
      join person p on p.id = b.person_id
      where b.birthday_date >= p.birth_date
        and ((b.birthday_date + time '12:00')::timestamp)::timestamptz >= p_since
        and ((b.birthday_date + time '12:00')::timestamp)::timestamptz < p_until;
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
        'COMPETITION_RESULT'::activity_timeline_kind as kind,
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
        cr.competition_type,
        cr.event_external_id as competition_event_external_id,
        cr.competition_external_id
      from (
        select * from competition_report(p_since::date, p_until::date, p_cohort_id, p_person_ids)
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
          select * from competition_report(p_since::date, p_until::date, p_cohort_id, p_person_ids)
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
        'COMPETITION_BRIEF'::activity_timeline_kind as kind,
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
        cb.competition_type,
        cb.event_external_id as competition_event_external_id,
        cb.competition_external_id
      from (
        select * from competition_brief(p_since::date, p_until::date, p_cohort_id, p_person_ids)
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

  if include_judging then
    return query
      with scoped_people as (
        select distinct p.id, p.name, p.csts_id, p.wdsf_id
        from person p
        where (exists (select 1 from current_tenant_membership where person_id = p.id)
           or exists (select 1 from current_tenant_trainer where person_id = p.id)
           or exists (select 1 from current_tenant_administrator where person_id = p.id))
          and (p_person_ids is null or p.id = any(p_person_ids))
          and (p_cohort_id is null or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
      ),
      federated_people as (
        select
          sp.id as person_id,
          sp.name as person_name,
          fp.id as federated_person_id,
          fp.federation
        from scoped_people sp
        join federated.person fp
          on (fp.federation = 'csts' and fp.external_id = sp.csts_id)
          or (fp.federation = 'wdsf' and fp.external_id = sp.wdsf_id)
      )
      select distinct
        ('judging:event:' || e.id || ':' || fp.person_id)::text as id,
        'JUDGING'::activity_timeline_kind as kind,
        ((e.start_date + time '12:00')::timestamp)::timestamptz as sort_at,
        e.start_date as activity_date,
        fp.person_id,
        fp.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_instance_id,
        e.federation,
        fp.federated_person_id,
        null::text as competitor_id,
        null::text as competitor_name,
        null::federated.competitor_type as competitor_type,
        e.id as competition_event_id,
        e.name as competition_event_name,
        coalesce(e.location, e.city) as competition_event_location,
        null::bigint as competition_id,
        e.start_date as competition_date,
        null::time as check_in_end,
        null::federated.category as category,
        null::text[] as dances,
        null::integer as participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        null::federated.competition_type as competition_type,
        e.external_id as competition_event_external_id,
        null::text as competition_external_id
      from federated.event_official eo
      join federated.event e on e.id = eo.event_id
      join federated_people fp on fp.federated_person_id = eo.person_id
      where eo.role = 'adjudicator'
        and e.start_date >= current_date
        and ((e.start_date + time '12:00')::timestamp)::timestamptz >= p_since
        and ((e.start_date + time '12:00')::timestamp)::timestamptz < p_until
      union all
      select
        ('judging:competition:' || c.id || ':' || fp.person_id)::text as id,
        'JUDGING'::activity_timeline_kind as kind,
        ((c.start_date + time '12:00')::timestamp)::timestamptz as sort_at,
        c.start_date as activity_date,
        fp.person_id,
        fp.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_instance_id,
        c.federation,
        fp.federated_person_id,
        null::text as competitor_id,
        null::text as competitor_name,
        null::federated.competitor_type as competitor_type,
        e.id as competition_event_id,
        e.name as competition_event_name,
        coalesce(e.location, e.city) as competition_event_location,
        c.id as competition_id,
        c.start_date as competition_date,
        null::time as check_in_end,
        cat as category,
        null::text[] as dances,
        null::integer as participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        c.competition_type,
        e.external_id as competition_event_external_id,
        c.external_id as competition_external_id
      from federated.competition_official co
      join federated.competition c on c.id = co.competition_id
      join federated.event e on e.id = c.event_id
      join federated.category cat on cat.id = c.category_id
      join federated_people fp on fp.federated_person_id = co.person_id
      where co.role = 'adjudicator'
        and c.start_date < current_date
        and ((c.start_date + time '12:00')::timestamp)::timestamptz >= p_since
        and ((c.start_date + time '12:00')::timestamp)::timestamptz < p_until;
  end if;
end;
$function$;

select verify_function('public.activity_timeline');
comment on function public.activity_timeline(
  timestamptz,
  timestamptz,
  bigint[],
  bigint,
  public.activity_timeline_kind[],
  public.event_type[]
) is '@behavior +queryField:resource:list -queryField:resource:connection';
grant all on function public.activity_timeline(
  timestamptz,
  timestamptz,
  bigint[],
  bigint,
  public.activity_timeline_kind[],
  public.event_type[]
) to anonymous;
