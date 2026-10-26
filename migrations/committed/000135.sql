--! Previous: sha1:83abb1b56c45413da3e329ed5c7e0e5924459980
--! Hash: sha1:2f0a885df1629c54968083fd32e50c0d420b65c5

--! split: 1-current.sql
do $$
begin
  if not exists (
    select 1 from pg_attribute
    where attrelid = (select typrelid from pg_type where oid = 'public.competition_participation_record'::regtype)
      and attname = 'final_gain' and not attisdropped
  ) then
    alter type public.competition_participation_record add attribute final_gain numeric(10,3);
  end if;
end;
$$;

grant execute on function federated.class_rank to anonymous;

--! Included functions/competition_reports.sql
create or replace function competition_brief(
  p_since date default (date_trunc('week', now())::date + 4),
  p_until date default (date_trunc('week', now())::date + 7),
  p_cohort_id bigint default null,
  p_person_ids bigint[] default null
) returns setof competition_participation_record
  language sql stable
as $$
  with scoped_people as materialized (
    select p.id, p.name
    from person p
    where (p.id = any(p_person_ids)
       or (p_person_ids is null and p.id in (
         select person_id from current_tenant_membership
         union all
         select person_id from current_tenant_trainer
         union all
         select person_id from current_tenant_administrator
       )))
      and (p_cohort_id is null
       or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
  ),
  federated_people as (
    select
      sp.id as person_id,
      sp.name as person_name,
      fp.id as federated_person_id,
      fp.federation
    from scoped_people sp
    join person_federation_link link on link.person_id = sp.id
    join federated.person fp
      on (fp.federation, fp.external_id) = (link.federation, link.external_id)
  )
  select
    fp.person_id,
    fp.person_name,
    fp.federation,
    fp.federated_person_id,
    c.id as competitor_id,
    c.name as competitor_name,
    c.competitor_type,
    e.id as event_id,
    e.name as event_name,
    coalesce(e.location, e.city) as event_location,
    comp.id as competition_id,
    comp.start_date as competition_date,
    comp.check_in_end,
    cat as category,
    dances.dances,
    comp.participants_total as participants,
    null::integer as ranking,
    null::integer as ranking_to,
    null::numeric(10,3) as point_gain,
    null::boolean as is_final,
    false as has_result,
    comp.competition_type,
    e.external_id as event_external_id,
    comp.external_id as competition_external_id,
    null::numeric(10,3) as final_gain
  from federated_people fp
  join federated.competitor_component cc on cc.person_id = fp.federated_person_id
  join federated.competitor c on c.id = cc.competitor_id
  join federated.competition_entry ce on ce.competitor_id = c.id and not ce.cancelled
  join federated.competition comp on comp.id = ce.competition_id
  join federated.event e on e.id = comp.event_id
  join federated.category cat on cat.id = comp.category_id
  left join lateral (
    select coalesce(array_agg(d.name order by dpd.dance_order), '{}'::text[]) as dances
    from federated.dance_program_dance dpd
    join federated.dance d on d.code = dpd.dance_code
    where dpd.program_id = cat.base_dance_program_id
  ) dances on true
  where comp.start_date >= p_since
    and comp.start_date < p_until
  order by
    comp.start_date,
    comp.check_in_end nulls last,
    fp.person_name,
    cat.discipline,
    cat.class,
    c.name;
$$;

create or replace function competition_report(
  p_since date default (date_trunc('week', now())::date - 2),
  p_until date default date_trunc('week', now())::date,
  p_cohort_id bigint default null,
  p_person_ids bigint[] default null
) returns setof competition_participation_record
  language sql stable
as $$
  with scoped_people as materialized (
    select p.id, p.name
    from person p
    where (p.id = any(p_person_ids)
       or (p_person_ids is null and p.id in (
         select person_id from current_tenant_membership
         union all select person_id from current_tenant_trainer
         union all select person_id from current_tenant_administrator
       )))
      and (p_cohort_id is null
       or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
  ),
  federated_people as (
    select
      sp.id as person_id,
      sp.name as person_name,
      fp.id as federated_person_id,
      fp.federation
    from scoped_people sp
    join person_federation_link link on link.person_id = sp.id
    join federated.person fp
      on (fp.federation, fp.external_id) = (link.federation, link.external_id)
  )
  select
    fp.person_id,
    fp.person_name,
    fp.federation,
    fp.federated_person_id,
    c.id as competitor_id,
    c.name as competitor_name,
    c.competitor_type,
    e.id as event_id,
    e.name as event_name,
    coalesce(e.location, e.city) as event_location,
    comp.id as competition_id,
    comp.start_date as competition_date,
    comp.check_in_end,
    cat as category,
    coalesce(dances.dances, '{}'::text[]) as dances,
    comp.participants_total as participants,
    cr.ranking,
    cr.ranking_to,
    cr.point_gain,
    cr.is_final,
    true as has_result,
    comp.competition_type,
    e.external_id as event_external_id,
    comp.external_id as competition_external_id,
    cr.final_gain
  from federated_people fp
  join federated.competitor_component cc on cc.person_id = fp.federated_person_id
  join federated.competitor c on c.id = cc.competitor_id
  join federated.competition_result cr on cr.competitor_id = c.id
  join federated.competition comp on comp.id = cr.competition_id
  join federated.event e on e.id = comp.event_id
  join federated.category cat on cat.id = comp.category_id
  left join lateral (
    select array_agg(d.name order by rd.dance_order) as dances
    from (
      select r.id
      from federated.competition_round r
      where r.competition_id = comp.id
      order by r.round_index desc, (r.round_key = 'F') desc, r.id desc
      limit 1
    ) r
    join federated.round_dance rd on rd.round_id = r.id
    join federated.dance d on d.code = rd.dance_code
  ) dances on true
  where comp.start_date >= p_since
    and comp.start_date < p_until
  order by
    comp.start_date,
    fp.person_name,
    cat.discipline,
    cat.class,
    cr.ranking,
    c.name;
$$;

comment on function competition_brief is '@behavior -*';
grant all on function competition_brief to anonymous;

comment on function competition_report is '@behavior -*';
grant all on function competition_report to anonymous;
--! EndIncluded functions/competition_reports.sql
--! Included functions/activity_timeline.sql
drop function if exists activity_timeline;
drop view if exists activity_timeline_item;

create or replace view activity_timeline_item as
select
  null::text as id,
  null::activity_timeline_kind as kind,
  null::timestamptz as sort_at,
  null::date as activity_date,
  null::bigint as person_id,
  null::text as person_name,
  null::bigint as event_attendance_id,
  null::bigint as event_id,
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
  null::numeric(10, 3) as final_gain,
  null::federated.competition_type as competition_type,
  null::text as competition_event_external_id,
  null::text as competition_external_id
where false;

comment on view activity_timeline_item is $$
@primaryKey id
@interface mode:single type:kind
@type EVENT_ATTENDANCE name:ActivityEventAttendance attributes:event_attendance_id,event_id
@type COMPETITION_BRIEF name:ActivityCompetitionBrief attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,check_in_end,category,dances,participants,competition_type,competition_event_external_id,competition_external_id
@type COMPETITION_RESULT name:ActivityCompetitionResult attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,category,dances,participants,ranking,ranking_to,point_gain,is_final,final_gain,competition_type,competition_event_external_id,competition_external_id
@type JUDGING name:ActivityJudging attributes:federation,federated_person_id,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,category,competition_type,competition_event_external_id,competition_external_id
@type BIRTHDAY name:ActivityBirthday
@foreignKey (person_id) references person (id)|@fieldName person|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_attendance_id) references event_registration (id)|@fieldName eventAttendance|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_id) references event (id)|@fieldName event|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@behavior -query:resource:list -query:resource:connection -query:resource:single
$$;

grant select on activity_timeline_item to anonymous;

create or replace function activity_timeline(p_since timestamp with time zone, p_until timestamp with time zone, p_person_ids bigint[] DEFAULT NULL::bigint[], p_cohort_id bigint DEFAULT NULL::bigint, p_kinds activity_timeline_kind[] DEFAULT NULL::activity_timeline_kind[], p_event_types event_type[] DEFAULT NULL::event_type[])
 returns setof activity_timeline_item
 language plpgsql
 stable
as $function$
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
        ei.id as event_id,
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
        null::numeric(10, 3) as final_gain,
        null::federated.competition_type as competition_type,
        null::text as competition_event_external_id,
        null::text as competition_external_id
      from event_registration ea
      join event ei on ei.id = ea.event_id
      join scoped_people sp on sp.id = ea.person_id
      where ea.registration_status = 'active'
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
        null::bigint as event_id,
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
        null::numeric(10, 3) as final_gain,
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
        null::bigint as event_id,
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
        cr.final_gain,
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
        null::bigint as event_id,
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
        null::numeric(10, 3) as final_gain,
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
        select p.id, p.name
        from person p
        where (p.id = any(p_person_ids)
           or (p_person_ids is null and (
             exists (select 1 from current_tenant_membership where person_id = p.id)
             or exists (select 1 from current_tenant_trainer where person_id = p.id)
             or exists (select 1 from current_tenant_administrator where person_id = p.id)
           )))
          and (p_cohort_id is null or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
      ),
      federated_people as (
        select
          sp.id as person_id,
          sp.name as person_name,
          fp.id as federated_person_id,
          fp.federation
        from scoped_people sp
        join person_federation_link link on link.person_id = sp.id
        join federated.person fp
          on (fp.federation, fp.external_id) = (link.federation, link.external_id)
      )
      select distinct
        ('judging:event:' || e.id || ':' || fp.person_id)::text as id,
        'JUDGING'::activity_timeline_kind as kind,
        ((e.start_date + time '12:00')::timestamp)::timestamptz as sort_at,
        e.start_date as activity_date,
        fp.person_id,
        fp.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_id,
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
        null::numeric(10, 3) as final_gain,
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
        null::bigint as event_id,
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
        null::numeric(10, 3) as final_gain,
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

comment on function activity_timeline is '@behavior +queryField:resource:list -queryField:resource:connection';
grant all on function activity_timeline to anonymous;
--! EndIncluded functions/activity_timeline.sql

--! Included policies/person.sql
select app_private.drop_policies('public.person');

create policy admin_all on person to administrator using (true);
create policy admin_myself on person for update
  using (id = any ((select current_person_ids())::bigint[]));
create policy view_tenant_or_trainer on person for select
  using (id in (select app_private.visible_person_ids()));
create policy view_active_couple_partner on person for select
  using (id in (
    select unnest(array[c.man_id, c.woman_id])
    from couple c
    where c.status = 'active'
  ));

create policy event_share_view on person for select to anonymous
  using (id in (
    select person_id from event_registration
    where registration_status = 'active' and person_id is not null
      and event_id = any ((select current_setting('jwt.claims.shared.event_ids', true))::bigint[])
    union all
    select person_id from event_trainer
    where event_id = any ((select current_setting('jwt.claims.shared.event_ids', true))::bigint[])
  ));
--! EndIncluded policies/person.sql

alter table federated.competitor add column if not exists divisions text[] not null default '{}';
comment on column federated.competitor.divisions is '@behavior -filterBy -orderBy';

update federated.competitor c set divisions = array[cache.content->>'division'], age_group = cache.content->>'age'
from crawler.frontier f
join crawler.json_response r on r.id = f.last_successful_response_id
join crawler.json_response_cache cache on cache.content_hash = r.content_hash
where f.federation = 'wdsf' and f.kind = 'couple' and c.id = 'wdsf:' || (cache.content->>'id')
  and cache.content->>'division' is not null
  and (c.divisions, c.age_group) is distinct from (array[cache.content->>'division'], cache.content->>'age');

--! Included views/competition_participation.sql
create or replace view federated.competition_participation
with (security_invoker = true) as
select r.competition_id, r.competitor_id, coalesce(e.cancelled, false) as cancelled
from federated.competition_result r
left join federated.competition_entry e using (competition_id, competitor_id)
union all
select e.competition_id, e.competitor_id, e.cancelled
from federated.competition_entry e
where not exists (
  select 1 from federated.competition_result r
  where (r.competition_id, r.competitor_id) = (e.competition_id, e.competitor_id)
);

comment on view federated.competition_participation is '@name competition_participation
@primaryKey competition_id,competitor_id
@foreignKey (competition_id) references federated.competition (id)|@fieldName competition|@foreignFieldName participations
@foreignKey (competitor_id) references federated.competitor (id)|@fieldName competitor|@foreignFieldName competitionParticipations
@behavior -single -list -connection -insert -update -delete';
grant select on federated.competition_participation to anonymous;
--! EndIncluded views/competition_participation.sql
--! Included views/couple_federation_link.sql
create or replace view public.couple_federation_link
with (security_invoker = true) as
with links as materialized (
  select * from public.person_federation_link
)
select c.id as couple_id, fc.id as competitor_id
from public.couple c
join links ml on ml.person_id = c.man_id
join federated.person mp on (mp.federation, mp.external_id) = (ml.federation, ml.external_id)
join federated.competitor_component mc on mc.person_id = mp.id and mc.role = 'lead'
join federated.competitor fc on fc.id = mc.competitor_id and fc.competitor_type = 'couple'
join federated.competitor_component wc on wc.competitor_id = fc.id and wc.role = 'follow'
join federated.person wp on wp.id = wc.person_id
join links wl on wl.person_id = c.woman_id
  and (wl.federation, wl.external_id) = (wp.federation, wp.external_id);

comment on view public.couple_federation_link is '@primaryKey couple_id,competitor_id
@foreignKey (couple_id) references public.couple (id)
@foreignKey (competitor_id) references federated.competitor (id)
@behavior -single -list -connection -insert -update -delete';
grant select on public.couple_federation_link to anonymous;
--! EndIncluded views/couple_federation_link.sql
--! Included functions/competition_suggestions.sql
drop function if exists competition_suggestions;
drop function if exists suggested_competition_matches;

create or replace function suggested_competition_matches(
  since date,
  until date default null,
  person_ids bigint[] default null,
  couple_id bigint default null,
  cohort_id bigint default null,
  event_ids bigint[] default null,
  registered_only boolean default false
) returns table (competition_id bigint, competitor_id text, local_person_id bigint, local_couple_id bigint, status text)
language sql stable as $$
  with scoped_people as materialized (
    select p.id
    from person p
    where (person_ids is not null or couple_id is not null or p.id in (
        select person_id from current_tenant_membership
        union all
        select person_id from current_tenant_trainer
        union all
        select person_id from current_tenant_administrator
      ))
      and (person_ids is null or p.id = any(person_ids))
      and (cohort_id is null or exists (
        select 1 from current_cohort_membership cm
        where cm.person_id = p.id and cm.cohort_id = suggested_competition_matches.cohort_id
      ))
      and (couple_id is null or exists (
        select 1 from couple c
        where c.id = suggested_competition_matches.couple_id and p.id in (c.man_id, c.woman_id)
      ))
  ), scoped_couples as materialized (
    select c.id
    from couple c
    where c.status = 'active'
      and (couple_id is null or c.id = couple_id)
      and (c.man_id in (select id from scoped_people) or c.woman_id in (select id from scoped_people))
  ), scoped_competitors as materialized (
    select fc.id, fc.federation, fc.competitor_type, array(
      select distinct case
        when license.discipline::text like 'pd_%' then 'Professional'
        when license.discipline = 'hiphop' then 'HipHop'
        when license.discipline in ('breaking', 'caribbean', 'stage', 'smooth', 'disco')
          then initcap(license.discipline::text)
        else 'General'
      end
      from federated.person_license license
      where license.person_id = fp.id and license.federation = 'wdsf'
        and license.kind = 'athlete' and license.discipline <> 'unknown'
    ) as divisions, fp.age_group_code, link.person_id as local_person_id, null::bigint as local_couple_id
    from person_federation_link link
    join federated.person fp on (fp.federation, fp.external_id) = (link.federation, link.external_id)
    join federated.competitor_component member on member.person_id = fp.id
    join federated.competitor fc on fc.id = member.competitor_id
    where couple_id is null and fc.competitor_type = 'solo'
      and link.person_id = any(array(select id from scoped_people))
    union
    select fc.id, fc.federation, fc.competitor_type, fc.divisions, fc.age_group_code,
      null::bigint, link.couple_id
    from couple_federation_link link
    join federated.competitor fc on fc.id = link.competitor_id
    where link.couple_id = any(array(select id from scoped_couples))
  ), profiles as materialized (
    select sc.local_person_id, sc.local_couple_id, sc.id, sc.federation, sc.competitor_type, sc.divisions, sc.age_group_code,
      count(*) = count(coalesce(extract(year from p.birth_date)::int, extract(year from fp.dob)::int, fp.birth_year)) as known_birth_years,
      min(coalesce(extract(year from p.birth_date)::int, extract(year from fp.dob)::int, fp.birth_year)) as older_birth_year,
      max(coalesce(extract(year from p.birth_date)::int, extract(year from fp.dob)::int, fp.birth_year)) as younger_birth_year,
      bool_and(coalesce(fp.gender = 'female', false)) as all_female,
      bool_and(coalesce(fp.gender = 'male', false)) as all_male
    from scoped_competitors sc
    join federated.competitor_component cc on cc.competitor_id = sc.id
    join federated.person fp on fp.id = cc.person_id
    left join person_federation_link link on (link.federation, link.external_id) = (fp.federation, fp.external_id)
    left join person p on p.id = link.person_id
    group by sc.local_person_id, sc.local_couple_id, sc.id, sc.federation, sc.competitor_type, sc.divisions, sc.age_group_code
  ), upcoming as materialized (
    select array_agg(c.id) as competition_ids, c.federation, c.competition_type, extract(year from c.start_date)::int as year,
      cat.series, cat.discipline, cat.class, cat.gender_group, cat.competitor_type, cat.age_group_code
    from federated.competition c
    join federated.event e on e.id = c.event_id
    join federated.category cat on cat.id = c.category_id
    where c.federation in ('csts', 'wdsf') and c.start_date >= since and (until is null or c.start_date < until)
      and (event_ids is null or c.event_id = any(event_ids))
      and c.status <> 'cancelled' and e.status <> 'cancelled'
    group by c.federation, c.competition_type, extract(year from c.start_date)::int, cat.id
  ), participations as materialized (
    select p.*
    from federated.competition_participation p
    where p.competition_id = any(array(select unnest(competition_ids) from upcoming))
      and p.competitor_id = any(array(select id from scoped_competitors))
  ), source_profiles as materialized (
    select p.*, source.series, source.discipline, source.class, source.age_group_code as reported_age
    from profiles p
    join federated.competitor_category_progress progress on progress.competitor_id = p.id
    join federated.category source on source.id = progress.category_id
      and source.competitor_type = p.competitor_type
    where p.federation = 'csts'
    union all
    -- WDSF divisions are independent of ČSTS proficiency classes and progress.
    select p.*, division, null, null, p.age_group_code
    from profiles p cross join unnest(p.divisions) division
    where p.federation = 'wdsf'
  ), category_profiles as materialized (
    select p.*, y.year, age.code as age_group, age.allowed_to_dance_in
    from source_profiles p
    cross join (select distinct year from upcoming) y
    left join lateral (
      select r.age_group_code as code
      from federated.couple_age_rule r
      where p.competitor_type = 'couple' and p.known_birth_years
        and r.division = 'General' and r.age_group_code <> 'Under_21'
        and r.older_age @> (y.year - p.older_birth_year)
        and r.younger_age @> (y.year - p.younger_birth_year)
      order by lower(r.older_age) desc nulls last, upper(r.older_age)
      limit 1
    ) couple_age on true
    left join federated.age_group person_age on p.competitor_type = 'solo' and p.known_birth_years
      and person_age.person_age @> (y.year - p.older_birth_year)
    join federated.age_group age on age.code = case
      when p.known_birth_years then coalesce(couple_age.code, person_age.code)
      when y.year = extract(year from current_date)::int then p.reported_age
    end
  ), eligible as (
  select distinct competition_id, p.id, p.local_person_id, p.local_couple_id
  from upcoming target
  join category_profiles p on p.year = target.year and p.federation = target.federation
    and (p.series = target.series or (p.federation = 'wdsf' and target.series = 'Any'))
    and (p.federation = 'wdsf' or p.discipline = target.discipline)
    and p.competitor_type = target.competitor_type
  cross join lateral unnest(target.competition_ids) competition_id
  where (target.gender_group in ('', 'mixed', 'SOLO')
      or (target.gender_group in ('female', 'F', '1 F') and p.all_female)
      or (target.gender_group in ('male', 'M', '1 M') and p.all_male))
    -- ČSTS leagues require B or higher, even though their categories say Open.
    and (p.federation <> 'csts' or target.competition_type not in ('league', 'super_league')
      or p.class in ('B', 'A', 'M', 'S'))
    and (p.federation = 'wdsf' or
      target.class in ('', 'Open') or target.class = p.class
      or (federated.class_rank(p.class) > 0
        and federated.class_rank(p.class) = federated.class_rank(target.class))
      or (target.class like '%-%'
        and federated.class_rank(split_part(target.class, '-', 1)) > 0
        and federated.class_rank(split_part(target.class, '-', 2)) > 0
        -- A range cannot cross between medal and letter classes.
        and (federated.class_rank(split_part(target.class, '-', 1)) > 10)
          = (federated.class_rank(split_part(target.class, '-', 2)) > 10)
        and federated.class_rank(p.class) between
          federated.class_rank(split_part(target.class, '-', 1)) and
          federated.class_rank(split_part(target.class, '-', 2)))
    )
    and (target.age_group_code = p.age_group or target.age_group_code = any(p.allowed_to_dance_in))
    -- Adult's allowances include U21; that is only valid below its upper age limit.
    and (target.age_group_code <> 'Under_21' or
      (p.known_birth_years and target.year - p.older_birth_year < 21) or
      (not p.known_birth_years and p.age_group in ('Youth', 'Under_21')))
  )
  select eligible.*, case
    when participation.cancelled then 'cancelled'
    when participation.competition_id is not null then 'registered'
    else 'suggested'
  end
  from eligible
  left join participations participation
    on (participation.competition_id, participation.competitor_id) = (eligible.competition_id, eligible.id)
  where not registered_only
  union
  select participation.competition_id, sc.id, sc.local_person_id, sc.local_couple_id,
    case when participation.cancelled then 'cancelled' else 'registered' end
  from scoped_competitors sc
  join participations participation on participation.competitor_id = sc.id
  where not registered_only or not participation.cancelled;
$$;

comment on function suggested_competition_matches is '@behavior -*';
grant execute on function suggested_competition_matches to anonymous;

-- Return the local competitors as rows so callers can summarize or inspect them.
create or replace function competition_suggestions(
  event_ids bigint[],
  since date default current_date,
  until date default null,
  person_ids bigint[] default null,
  couple_id bigint default null,
  cohort_id bigint default null,
  registered_only boolean default false
) returns table (
  competition federated.competition,
  person public.person,
  couple public.couple,
  status text
)
language plpgsql stable as $$
begin
  if event_ids is null then
    raise exception 'event_ids is required' using errcode = '22004';
  end if;
  return query
  select c, p, partnership, m.status
  from (
    select competition_id, local_person_id, local_couple_id, case
      when bool_or(matches.status = 'registered') then 'registered'
      when bool_or(matches.status = 'cancelled') then 'cancelled'
      else 'suggested'
    end as status
    from suggested_competition_matches(since, until, person_ids, couple_id, cohort_id, event_ids, registered_only)
      matches
    group by competition_id, local_person_id, local_couple_id
  ) m
  join federated.competition c on c.id = m.competition_id
  left join public.person p on p.id = m.local_person_id
  left join public.couple partnership on partnership.id = m.local_couple_id;
end;
$$;

comment on function competition_suggestions is '@behavior +list -connection';
grant execute on function competition_suggestions to anonymous;
--! EndIncluded functions/competition_suggestions.sql

alter table public.event add column if not exists cover_image_id bigint;
alter table public.event drop constraint if exists event_cover_image_fk;
alter table public.event add constraint event_cover_image_fk
  foreign key (tenant_id, cover_image_id) references public.file (tenant_id, id)
  on delete set null (cover_image_id);
comment on constraint event_cover_image_fk on public.event is '@fieldName coverImage';
create index if not exists event_cover_image_idx on public.event (tenant_id, cover_image_id)
  where cover_image_id is not null;

--! Included functions/accessible_event_ids.sql
create or replace function app_private.accessible_event_ids() returns setof bigint
language sql stable security definer
set search_path = pg_catalog, public, pg_temp
as $$
  with recursive attended(id) as (
    select event_id from public.event_registration
    where tenant_id = (select public.current_tenant_id())
      and person_id = any((select public.current_person_ids())::bigint[])
      and registration_status = 'active'
    union
    select child.id from public.event child
    join attended parent on child.parent_id = parent.id
    where child.parent_id is not null
      and child.tenant_id = (select public.current_tenant_id())
  )
  -- Scan events once; their primary key already makes the result unique.
  select id from public.event
  where tenant_id = (select public.current_tenant_id())
    and (
      is_public
      or (is_visible and (select pg_has_role(current_setting('role'), 'member', 'member')))
      or (manager_person_ids && (select current_person_ids()))
      or id = any ((select current_setting('jwt.claims.shared.event_ids', true))::bigint[])
      or id in (select id from attended)
    );
$$;

grant execute on function app_private.accessible_event_ids to anonymous;
--! EndIncluded functions/accessible_event_ids.sql
--! Included functions/visible_file_ids.sql
create or replace function app_private.visible_file_ids()
  returns setof bigint
  language sql stable
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  -- Start with cover candidates rather than probing every accessible event.
  with event_covers as materialized (
    select id, cover_image_id from event
    where tenant_id = (select current_tenant_id())
      and cover_image_id is not null
  )
  select id
  from file
  where tenant_id = (select current_tenant_id())
    and is_public

  union all

  select f.file_id
  from announcement_attachment f
  join app_private.visible_announcement_ids() a(id) on a.id = f.announcement_id
  where f.tenant_id = (select current_tenant_id())

  union all

  select f.file_id
  from article_attachment f
  join aktuality a on id = f.aktuality_id and a.tenant_id = f.tenant_id
  where a.tenant_id = (select current_tenant_id())
    and a.is_visible

  union all

  select image.file_id
  from location_image image
  join location
    on location.tenant_id = image.tenant_id
    and location.id = image.location_id
  where image.tenant_id = (select current_tenant_id())

  union all

  select cover_image_id
  from location
  where tenant_id = (select current_tenant_id())
    and cover_image_id is not null

  union all

  select cover_image_id from event_covers
  where id = any (array(select app_private.accessible_event_ids()));
$$;

grant execute on function app_private.visible_file_ids() to anonymous;
--! EndIncluded functions/visible_file_ids.sql
--! Included functions/set_lesson_request.sql
drop function if exists set_lesson_demand;
drop function if exists set_lesson_request;
drop function if exists app_private.set_lesson_demand;
drop function if exists app_private.set_lesson_request;

create function app_private.set_lesson_request(
  registration event_registration,
  trainer_id bigint,
  lesson_count integer
) returns event_lesson_request
language plpgsql strict
as $$
declare
  trainer event_trainer;
  lesson_request event_lesson_request;
  other_lessons bigint;
begin
  select * into trainer
  from event_trainer
  where id = $2 and event_id = registration.event_id and tenant_id = current_tenant_id()
  for update;

  if not found then
    raise exception 'TRAINER_NOT_FOUND' using errcode = '28000';
  end if;

  if $3 = 0 then
    delete from event_lesson_request eld
    where eld.registration_id = registration.id and eld.trainer_id = $2;

    return null;
  end if;

  if trainer.lessons_offered = 0 then
    raise exception 'LESSONS_NOT_OFFERED' using errcode = '28000';
  end if;

  if trainer.lessons_offered is not null then
    select coalesce(sum(eld.lesson_count), 0) into other_lessons
    from event_lesson_request eld
    join event_registration other_registration
      on other_registration.id = eld.registration_id
     and other_registration.registration_status = 'active'
    where eld.trainer_id = $2 and eld.registration_id <> registration.id;

    if $3 > trainer.lessons_offered - other_lessons then
      raise exception 'LESSON_LIMIT_EXCEEDED' using errcode = '22023';
    end if;
  end if;

  insert into event_lesson_request (registration_id, trainer_id, lesson_count)
  values (registration.id, $2, $3)
  on conflict on constraint event_lesson_request_registration_id_trainer_id_key do update set lesson_count = $3
  returning * into lesson_request;

  return lesson_request;
end;
$$;

create function set_lesson_request(
  registration_id bigint,
  trainer_id bigint,
  lesson_count integer
) returns event_lesson_request
language plpgsql strict security definer
set search_path = pg_catalog, public, pg_temp
as $$
declare
  registration event_registration;
  instance event;
  is_manager boolean;
  is_self boolean;
begin
  select * into registration
  from event_registration
  where id = $1;

  if not found then
    raise exception 'REGISTRATION_NOT_FOUND' using errcode = '28000';
  end if;

  select * into instance
  from event
  where id = registration.event_id and tenant_id = current_tenant_id();

  if not found then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '28000';
  end if;

  is_self := registration.person_id = any(current_person_ids())
    or registration.couple_id = any(current_couple_ids());
  is_manager := pg_has_role(current_setting('role'), 'administrator', 'member')
    or (pg_has_role(current_setting('role'), 'trainer', 'member') and app_private.can_trainer_edit_event(instance.id));

  if not coalesce(is_self, false) and not coalesce(is_manager, false) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  if instance.is_locked and not is_manager then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;

  return app_private.set_lesson_request(registration, $2, $3);
end;
$$;

grant all on function set_lesson_request to anonymous;

create function set_lesson_demand(
  instance_registration_id bigint,
  instance_trainer_id bigint,
  lesson_count integer
) returns event_lesson_request
language sql strict
as $$
  select public.set_lesson_request($1, $2, $3);
$$;

comment on function set_lesson_demand is E'@deprecated Use setLessonRequest.\n@resultFieldName eventLessonDemand';
grant execute on function set_lesson_demand to anonymous;
--! EndIncluded functions/set_lesson_request.sql
--! Included functions/set_event_registration.sql
drop function if exists set_event_instance_registration;
drop function if exists set_event_registration;
drop function if exists register_to_event;
drop function if exists app_private.set_event_instance_registration;
drop type if exists event_registration_person_input;

create type event_registration_person_input as (
  first_name text,
  last_name text,
  gender gender_type,
  nationality text,
  email text,
  phone text,
  birth_date date,
  csts_id integer,
  wdsf_id integer
);

create function set_event_registration(
  event_id bigint,
  person_id bigint,
  couple_id bigint,
  is_registered boolean,
  note text default null,
  lesson_trainer_ids bigint[] default null,
  lesson_counts integer[] default null,
  person event_registration_person_input default null
) returns event_registration
language plpgsql security definer
set search_path = pg_catalog, public, pg_temp
as $$
declare
  target_instance event;
  registration event_registration;
  registration_found boolean;
  is_self boolean;
  is_manager boolean;
  registration_source event_registration_source;
  required_capacity integer;
  remaining_capacity integer;
  request record;
begin
  if is_registered is null or num_nonnulls(person_id, couple_id, person) <> 1
    or (person is distinct from null and not is_registered) then
    raise exception 'INVALID_REGISTRANT' using errcode = '22023';
  end if;

  if num_nonnulls(lesson_trainer_ids, lesson_counts) = 1
    or cardinality(lesson_trainer_ids) <> cardinality(lesson_counts)
    or exists (
      select 1
      from unnest(lesson_trainer_ids, lesson_counts) demand(trainer_id, lesson_count)
      where demand.trainer_id is null
        or demand.lesson_count is null
        or demand.lesson_count < 0
    ) then
    raise exception 'INVALID_LESSON_DEMANDS' using errcode = '22023';
  end if;
  if not is_registered and lesson_trainer_ids is not null then
    raise exception 'INVALID_LESSON_DEMANDS' using errcode = '22023';
  end if;

  select * into target_instance
  from event
  where id = event_id
    and tenant_id = current_tenant_id()
  for update;

  if not found then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '22023';
  end if;

  is_manager := pg_has_role(current_setting('role'), 'administrator', 'member')
    or (pg_has_role(current_setting('role'), 'trainer', 'member') and app_private.can_trainer_edit_event(event_id));

  if person is distinct from null then
    if nullif(btrim(person.first_name), '') is null
      or nullif(btrim(person.last_name), '') is null
      or person.gender is null
      or nullif(btrim(person.nationality), '') is null
      or person.email is null or person.email !~ '^[^[:space:]@]+@[^[:space:]@]+\.[^[:space:]@]+$'
      or nullif(btrim(person.phone), '') is null
      or person.birth_date > current_date
      or person.csts_id <= 0 or person.wdsf_id <= 0 then
      raise exception 'INVALID_PERSON_DETAILS' using errcode = '22023';
    end if;

    select r.* into registration
    from event_registration r
    join person p on p.id = r.person_id
    where r.event_id = set_event_registration.event_id
      and r.tenant_id = current_tenant_id()
      and r.created_by is not distinct from current_user_id()
      and r.registration_status = 'active'
      and r.parent_registration_id is null
      and r.note is not distinct from set_event_registration.note
      and row(
        p.first_name, p.last_name, p.gender, p.nationality,
        p.email, p.phone, p.birth_date, p.csts_id, p.wdsf_id
      ) is not distinct from row(
        btrim((set_event_registration.person).first_name), btrim((set_event_registration.person).last_name),
        (set_event_registration.person).gender, (set_event_registration.person).nationality,
        btrim((set_event_registration.person).email), btrim((set_event_registration.person).phone),
        (set_event_registration.person).birth_date, (set_event_registration.person).csts_id,
        (set_event_registration.person).wdsf_id
      )
      and coalesce((
        select jsonb_object_agg(d.trainer_id, d.lesson_count)
        from event_lesson_request d where d.registration_id = r.id
      ), '{}'::jsonb) = coalesce((
        select jsonb_object_agg(d.trainer_id, d.lesson_count)
        from unnest(lesson_trainer_ids, lesson_counts) d(trainer_id, lesson_count)
        where d.lesson_count > 0
      ), '{}'::jsonb)
    order by r.id
    limit 1;

    if found then
      return registration;
    end if;

    insert into person (
      first_name, last_name, gender, nationality, email, phone, birth_date, csts_id, wdsf_id
    ) values (
      btrim((set_event_registration.person).first_name), btrim((set_event_registration.person).last_name),
      (set_event_registration.person).gender, (set_event_registration.person).nationality,
      btrim((set_event_registration.person).email), btrim((set_event_registration.person).phone),
      (set_event_registration.person).birth_date, (set_event_registration.person).csts_id,
      (set_event_registration.person).wdsf_id
    )
    returning id into person_id;

    is_self := not is_manager;
  else
    is_self := coalesce(person_id = any(current_person_ids()), false)
      or coalesce(couple_id = any(current_couple_ids()), false);
  end if;

  if not is_self and not is_manager then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;
  if target_instance.is_locked and not is_manager then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  registration_source := case when is_self
    then 'self'::event_registration_source
    else 'manager'::event_registration_source
  end;

  select * into registration
  from event_registration r
  where r.event_id = set_event_registration.event_id
    and r.person_id is not distinct from set_event_registration.person_id
    and r.couple_id is not distinct from set_event_registration.couple_id
  order by (r.registration_status = 'active') desc, (r.parent_registration_id is null) desc, r.id desc
  limit 1;
  registration_found := found;

  if not is_registered then
    if not registration_found then
      raise exception 'REGISTRATION_NOT_FOUND' using errcode = '22023';
    end if;

    update event_registration
    set registration_status = 'cancelled',
        target_cohort_id = null,
        source = registration_source
    where id = registration.id or parent_registration_id = registration.id;

    select * into registration from event_registration where id = registration.id;
    return registration;
  end if;

  if registration_found and registration.registration_status = 'active' then
    if note is not null then
      update event_registration set note = set_event_registration.note where id = registration.id
      returning * into registration;
    end if;
  else
    if not is_manager and (
      target_instance.is_cancelled
        or target_instance.until <= now()
        or not (
          coalesce(target_instance.is_public, false)
          or (
            coalesce(target_instance.is_visible, false)
            and current_tenant_id() = any(my_tenants_array())
          )
        )
    ) then
      raise exception 'NOT_ALLOWED' using errcode = '28000';
    end if;

    if not is_manager then
      remaining_capacity := event_remaining_person_spots(target_instance);
      required_capacity := case
        when target_instance.capacity_unit = 'people' and couple_id is not null then 2
        when target_instance.capacity_unit = 'registrations' and exists (
          select 1 from event_registration parent
          where parent.id = registration.parent_registration_id and parent.registration_status = 'active'
        ) then 0
        else 1
      end;
      if couple_id is not null then
        required_capacity := required_capacity - (
          select count(*)::integer from event_registration r
          join couple c on c.id = set_event_registration.couple_id and r.person_id in (c.man_id, c.woman_id)
          where r.event_id = set_event_registration.event_id and r.registration_status = 'active'
            and (target_instance.capacity_unit = 'people' or r.parent_registration_id is null)
        );
      end if;
      if remaining_capacity is not null and remaining_capacity < required_capacity then
        raise exception 'CAPACITY_EXCEEDED' using errcode = '22023';
      end if;
    end if;

    if registration_found then
      update event_registration
      set registration_status = 'active',
          target_cohort_id = null,
          source = registration_source,
          parent_registration_id = (
            select parent.id from event_registration parent
            where parent.id = registration.parent_registration_id and parent.registration_status = 'active'
          )
      where id = registration.id;

      if note is not null then
        update event_registration set note = set_event_registration.note where id = registration.id;
      end if;
    else
      insert into event_registration (
        event_id, person_id, couple_id, source, status, note
      ) values (
        event_id, set_event_registration.person_id, set_event_registration.couple_id, registration_source,
        case when set_event_registration.person_id is not null then 'unknown'::attendance_type end,
        set_event_registration.note
      ) returning * into registration;
    end if;
  end if;

  if couple_id is not null then
    perform app_private.ensure_couple_person_registrations(registration.id);
  end if;

  select * into registration from event_registration where id = registration.id;

  if lesson_trainer_ids is not null then
    delete from event_lesson_request
    where registration_id = registration.id
      and trainer_id <> all(lesson_trainer_ids);

    for request in
      select demand.trainer_id, demand.lesson_count
      from unnest(lesson_trainer_ids, lesson_counts)
        demand(trainer_id, lesson_count)
      order by demand.trainer_id
    loop
      perform app_private.set_lesson_request(registration, request.trainer_id, request.lesson_count);
    end loop;
  end if;

  if person is distinct from null then
    insert into person_invitation (person_id, email, event_id)
    values (registration.person_id, btrim((set_event_registration.person).email), event_id);
  end if;

  return registration;
end;
$$;

grant execute on function set_event_registration to anonymous;

create function set_event_instance_registration(
  p_instance_id bigint,
  p_person_id bigint,
  p_couple_id bigint,
  p_is_registered boolean,
  p_note text default null,
  p_lesson_trainer_ids bigint[] default null,
  p_lesson_counts integer[] default null,
  p_person event_registration_person_input default null
) returns event_registration
language sql
as $$
  select public.set_event_registration($1, $2, $3, $4, $5, $6, $7, $8);
$$;

comment on function set_event_instance_registration is E'@deprecated Use setEventRegistration.\n@resultFieldName eventInstanceRegistration';
grant execute on function set_event_instance_registration to anonymous;
--! EndIncluded functions/set_event_registration.sql

alter table public.person add column if not exists image_id bigint;
alter table public.person drop constraint if exists person_image_fk;
alter table public.person add constraint person_image_fk
  foreign key (image_id) references public.file (id) on delete set null;
comment on constraint person_image_fk on public.person is '@fieldName image';
create index if not exists person_image_idx on public.person (image_id) where image_id is not null;

alter table public.couple add column if not exists image_id bigint;
alter table public.couple drop constraint if exists couple_image_fk;
alter table public.couple add constraint couple_image_fk
  foreign key (image_id) references public.file (id) on delete set null;
comment on constraint couple_image_fk on public.couple is '@fieldName image';
create index if not exists couple_image_idx on public.couple (image_id) where image_id is not null;

alter table public.cohort add column if not exists image_id bigint;
alter table public.cohort drop constraint if exists cohort_image_fk;
alter table public.cohort add constraint cohort_image_fk
  foreign key (tenant_id, image_id) references public.file (tenant_id, id)
  on delete set null (image_id);
comment on constraint cohort_image_fk on public.cohort is '@fieldName image';
create index if not exists cohort_image_idx on public.cohort (tenant_id, image_id) where image_id is not null;

--! Included policies/file.sql
select app_private.drop_policies('public.file');
create policy admin_all on file to administrator
  using (tenant_id = (select current_tenant_id()));
create policy uploader_manage on file
  using (tenant_id = (select current_tenant_id())
    and uploaded_by = (select current_user_id()));
create policy visible on file for select
  using (
    (tenant_id = (select current_tenant_id())
      and id in (select app_private.visible_file_ids()))
    -- These uncorrelated subqueries use entity RLS, including share and partner access.
    or id in (
      select image_id from person where image_id is not null
      union all
      select image_id from couple where image_id is not null
      union all
      select image_id from cohort where image_id is not null
    )
  );

select app_private.drop_policies('public.announcement_attachment');
create policy current_tenant on announcement_attachment as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on announcement_attachment to administrator
  using (true);
create policy trainer_manage on announcement_attachment to trainer
  using (exists (
    select 1 from announcement a
    where a.id = announcement_attachment.announcement_id
      and a.author_id = (select current_user_id())));
create policy member_view on announcement_attachment for select
  using (exists (select 1 from announcement a where a.id = announcement_attachment.announcement_id));

select app_private.drop_policies('public.article_attachment');
create policy current_tenant on article_attachment as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on article_attachment to administrator
  using (true);
create policy public_view on article_attachment for select
  using (exists (select 1 from aktuality a where a.id = article_attachment.aktuality_id));
--! EndIncluded policies/file.sql

--! Included functions/competitor_candidates.sql
create or replace function app_private.competitor_category_candidates(
  age_groups text[] default null,
  classes text[] default null,
  disciplines text[] default null
) returns table(
  competitor_id text,
  age_group_code text, class text, discipline text,
  age_matches boolean, class_matches boolean, discipline_matches boolean
)
language sql stable as $$
  select p.competitor_id, cat.age_group_code, cat.class, cat.discipline,
    coalesce(cardinality(age_groups), 0) = 0 or cat.age_group_code = any(
      array(select federated.resolve_age_group(value) from unnest(age_groups) value)
    ),
    coalesce(cardinality(classes), 0) = 0 or cat.class = any(classes),
    coalesce(cardinality(disciplines), 0) = 0 or cat.discipline = any(disciplines)
  from federated.competitor_category_progress p
  join federated.category cat on cat.id = p.category_id;
$$;

grant execute on function app_private.competitor_category_candidates to anonymous;
--! EndIncluded functions/competitor_candidates.sql
--! Included functions/federation_competitor_facets.sql
create or replace function federated.federation_competitor_facets(
  federation federated.federation,
  search text default null,
  competitor_types federated.competitor_type[] default null,
  age_groups text[] default null,
  classes text[] default null,
  disciplines text[] default null,
  competed_since date default null
) returns table(facet text, value text, count integer)
language sql stable as $$
  with candidates as materialized (
    select c.id, c.competitor_type::text as competitor_type,
      p.age_group_code, p.class, p.discipline,
      coalesce(cardinality(competitor_types), 0) = 0 or c.competitor_type = any(competitor_types) as type_matches,
      -- Missing progress matches only category filters that are absent.
      coalesce(p.age_matches, coalesce(cardinality(age_groups), 0) = 0) as age_matches,
      coalesce(p.class_matches, coalesce(cardinality(classes), 0) = 0) as class_matches,
      coalesce(p.discipline_matches, coalesce(cardinality(disciplines), 0) = 0) as discipline_matches
    from federated.competitor c
    left join app_private.competitor_category_candidates(age_groups, classes, disciplines) p
      on p.competitor_id = c.id
    where c.federation = federation.code
      and (nullif(trim(search), '') is null or c.external_id::text = trim(search) or c.id = trim(search)
        or c.search_name like '%' || replace(replace(replace(
          app_private.normalize_name(trim(search)), '!', '!!'), '%', '!%'), '_', '!_') || '%' escape '!')
      and (competed_since is null or (c.latest_result_at >= competed_since
        and (c.latest_result_at <= current_date or exists (
          select 1 from federated.competition_result r
          join federated.competition comp on comp.id = r.competition_id
          where r.competitor_id = c.id and comp.start_date >= competed_since and comp.start_date <= current_date
        ))))
  )
  select option.facet, option.value, count(distinct c.id)::integer
  from candidates c
  cross join lateral (values
    ('type', c.competitor_type, c.age_matches and c.class_matches and c.discipline_matches),
    ('ageGroup', c.age_group_code, c.type_matches and c.class_matches and c.discipline_matches),
    ('class', c.class, c.type_matches and c.age_matches and c.discipline_matches),
    ('discipline', c.discipline, c.type_matches and c.age_matches and c.class_matches)
  ) option(facet, value, matches)
  where option.matches and option.value is not null
  group by option.facet, option.value
  order by option.facet, option.value;
$$;

comment on function federated.federation_competitor_facets is '@name federation_competitor_facets
@behavior +list -connection';
revoke all on function federated.federation_competitor_facets from public;
grant execute on function federated.federation_competitor_facets to anonymous;
--! EndIncluded functions/federation_competitor_facets.sql

--! Included functions/cohort_has_accessible_event.sql
create or replace function app_private.cohort_has_accessible_event(p_cohort_id bigint)
  returns boolean
  language sql stable security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  -- Most cohorts have a public or member-visible event. Check their indexed
  -- links before enumerating the caller's additional event access.
  select exists (
    select 1
    from event_target_cohort target
    join event e on e.tenant_id = target.tenant_id and e.id = target.event_id
    where target.tenant_id = (select current_tenant_id())
      and target.cohort_id = p_cohort_id
      and (e.is_public or (e.is_visible
        and (select pg_has_role(current_setting('role'), 'member', 'member'))))
  ) or exists (
    with targets as materialized (
      select event_id from event_target_cohort
      where tenant_id = (select current_tenant_id()) and cohort_id = p_cohort_id
    )
    select 1 from targets
    where event_id = any (array(select app_private.accessible_event_ids()))
  );
$$;

grant execute on function app_private.cohort_has_accessible_event(bigint) to anonymous;
--! EndIncluded functions/cohort_has_accessible_event.sql
--! Included policies/cohort.sql
select app_private.drop_policies('public.cohort');

create policy current_tenant on cohort as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on cohort to administrator
  using (true);
create policy public_view on cohort for select to anonymous
  using (is_visible or app_private.cohort_has_accessible_event(id));
--! EndIncluded policies/cohort.sql

alter table public.event add column if not exists series_info public.series_info;
comment on column public.event.series_info is
  '@behavior -attribute:insert -attribute:update -attribute:filterBy -attribute:orderBy';

drop function if exists public.event_instance_series_info;
drop function if exists public.event_series_info(public.event);

create or replace trigger _100_timestamps
  before insert or update on public.event for each row
  execute function app_private.tg__timestamps('manager_person_ids', 'stats', 'series_info');

--! Included functions/event_series_info.sql
create or replace function app_private.refresh_event_series_info(p_series_ids bigint[])
  returns void
  language plpgsql security definer
  set search_path = pg_catalog, public, pg_temp
as $$
begin
  if coalesce(cardinality(p_series_ids), 0) = 0 then
    return;
  end if;

  perform id from event_series
  where id = any(p_series_ids) order by id for no key update;

  with actual as (
    select member.id,
      row(series.id, series.name,
        row_number() over ordered::integer,
        count(*) over whole_series::integer,
        min(member.since) over whole_series,
        max(member.until) over whole_series)::series_info as info
    from event member
    join event_series series
      on series.tenant_id = member.tenant_id and series.id = member.series_id
    where series.id = any(p_series_ids)
    window whole_series as (partition by member.tenant_id, member.series_id),
      ordered as (whole_series order by member.since, member.id)
  )
  update event member set series_info = actual.info
  from actual
  where member.id = actual.id and member.series_info is distinct from actual.info;
end;
$$;

create or replace function app_private.tg_event__refresh_series_info()
  returns trigger
  language plpgsql security definer
  set search_path = pg_catalog, public, pg_temp
as $$
-- @plpgsql_check_options: oldtable = deleted_rows, newtable = changed_rows
declare
  series_ids bigint[];
begin
  if tg_op = 'INSERT' then
    select array_agg(distinct series_id) into series_ids
    from changed_rows where series_id is not null;
  elsif tg_op = 'DELETE' then
    select array_agg(distinct series_id) into series_ids
    from deleted_rows where series_id is not null;
  else
    select array_agg(distinct affected.series_id) into series_ids
    from deleted_rows old_row full join changed_rows new_row using (id)
    cross join lateral (values (old_row.series_id), (new_row.series_id)) affected(series_id)
    where row(old_row.id, old_row.tenant_id, old_row.series_id, old_row.since, old_row.until)
      is distinct from
      row(new_row.id, new_row.tenant_id, new_row.series_id, new_row.since, new_row.until)
      and affected.series_id is not null;
  end if;

  if tg_op <> 'DELETE' then
    if exists (select 1 from changed_rows
      where series_id is null and series_info is distinct from null) then
      update event member set series_info = null
      from changed_rows changed
      where member.id = changed.id and changed.series_id is null
        and member.series_info is distinct from null;
    end if;
  end if;

  perform app_private.refresh_event_series_info(series_ids);
  return null;
end;
$$;

create or replace function app_private.tg_event_series__refresh_info()
  returns trigger
  language plpgsql security definer
  set search_path = pg_catalog, public, pg_temp
as $$
-- @plpgsql_check_options: oldtable = deleted_rows, newtable = changed_rows
declare
  series_ids bigint[];
begin
  select array_agg(changed.id) into series_ids
  from changed_rows changed join deleted_rows previous using (id)
  where changed.name is distinct from previous.name;
  perform app_private.refresh_event_series_info(series_ids);
  return null;
end;
$$;

create or replace trigger _600_refresh_series_info_ins
  after insert on public.event
  referencing new table as changed_rows for each statement
  execute function app_private.tg_event__refresh_series_info();
create or replace trigger _600_refresh_series_info_del
  after delete on public.event
  referencing old table as deleted_rows for each statement
  execute function app_private.tg_event__refresh_series_info();
create or replace trigger _600_refresh_series_info_upd
  after update on public.event
  referencing old table as deleted_rows new table as changed_rows for each statement
  execute function app_private.tg_event__refresh_series_info();
create or replace trigger _600_refresh_series_info_name
  after update on public.event_series
  referencing old table as deleted_rows new table as changed_rows for each statement
  execute function app_private.tg_event_series__refresh_info();
--! EndIncluded functions/event_series_info.sql

select app_private.refresh_event_series_info(array(select id from public.event_series));
update public.event set series_info = null
where series_id is null and series_info is distinct from null;
