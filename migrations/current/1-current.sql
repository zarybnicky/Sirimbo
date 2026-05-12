do $$
begin
  if to_regtype('federated.competition_type') is null then
    create type federated.competition_type as enum (
      'cup',
      'ranking',
      'league',
      'championship',
      'top_level',
      'super_league',
      'g_cup',
      'unknown'
    );
  end if;
end
$$;

create or replace function app_private.normalize_name(text) returns text as $$
  select lower(public.unaccent('public.unaccent', $1));
$$ language sql immutable parallel safe strict;

do $$
begin
  if not exists (
    select 1
    from pg_enum e
    join pg_type t on t.oid = e.enumtypid
    join pg_namespace n on n.oid = t.typnamespace
    where n.nspname = 'federated'
      and t.typname = 'competition_type'
      and e.enumlabel = 'unknown'
  ) then
    alter type federated.competition_type add value 'unknown';
  end if;
end
$$;

alter table federated.competition
  add column if not exists competition_type federated.competition_type;

do $$
begin
  if exists (
    select 1
    from information_schema.columns
    where table_schema = 'federated'
      and table_name = 'competition'
      and column_name = 'competition_type'
      and udt_schema <> 'federated'
  ) then
    alter table federated.competition
      alter column competition_type type federated.competition_type
      using case competition_type::text
        when 'Cup' then 'cup'
        when 'Ranking' then 'ranking'
        when 'League' then 'league'
        when 'Championship' then 'championship'
        when 'TopLevel' then 'top_level'
        when 'SuperLeague' then 'super_league'
        when 'GCup' then 'g_cup'
        when 'cup' then 'cup'
        when 'ranking' then 'ranking'
        when 'league' then 'league'
        when 'championship' then 'championship'
        when 'top_level' then 'top_level'
        when 'super_league' then 'super_league'
        when 'g_cup' then 'g_cup'
        when 'Unknown' then 'unknown'
        when 'unknown' then 'unknown'
        else null
      end::federated.competition_type;
  end if;
end
$$;

comment on column federated.competition.competition_type is
  'Federation-provided competition type/grade, for example CSTS Cup, Ranking, League, Championship.';

do $$
begin
  if not exists (
    select 1
    from pg_type t
    join pg_attribute a on a.attrelid = t.typrelid
    where t.oid = 'public.competition_participation_record'::regtype
      and a.attname = 'competition_type'
      and not a.attisdropped
  ) then
    alter type public.competition_participation_record
      add attribute competition_type federated.competition_type;
  elsif exists (
    select 1
    from pg_type t
    join pg_attribute a on a.attrelid = t.typrelid
    where t.oid = 'public.competition_participation_record'::regtype
      and a.attname = 'competition_type'
      and a.atttypid <> 'federated.competition_type'::regtype
      and not a.attisdropped
  ) then
    alter type public.competition_participation_record
      alter attribute competition_type type federated.competition_type cascade;
  end if;
end
$$;

drop function if exists public.activity_timeline(
  timestamptz,
  timestamptz,
  bigint[],
  bigint,
  public.activity_timeline_kind[],
  public.event_type[]
);

drop function if exists public.activity_timeline(
  timestamptz,
  timestamptz,
  bigint[],
  bigint,
  public.activity_timeline_kind[]
);

drop view if exists public.activity_timeline_item;

do $$
begin
  if to_regtype('public.activity_timeline_kind') is null then
    create type public.activity_timeline_kind as enum (
      'EVENT_ATTENDANCE',
      'COMPETITION_BRIEF',
      'COMPETITION_RESULT'
    );
  end if;
end
$$;

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
  null::federated.competition_type as competition_type
where false;

comment on view public.activity_timeline_item is $$
@primaryKey id
@interface mode:single type:kind
@type EVENT_ATTENDANCE name:ActivityEventAttendance attributes:event_attendance_id,event_instance_id
@type COMPETITION_BRIEF name:ActivityCompetitionBrief attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,check_in_end,category,dances,participants,competition_type
@type COMPETITION_RESULT name:ActivityCompetitionResult attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,category,dances,participants,ranking,ranking_to,point_gain,is_final,competition_type
@foreignKey (person_id) references person (id)|@fieldName person|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_attendance_id) references event_attendance (id)|@fieldName eventAttendance|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_instance_id) references event_instance (id)|@fieldName eventInstance|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@behavior -query:resource:list -query:resource:connection -query:resource:single
$$;

grant select on public.activity_timeline_item to anonymous;

drop function if exists public.activity_timeline(
  timestamptz,
  timestamptz,
  bigint[],
  bigint,
  public.activity_timeline_kind[]
);

create or replace function public.activity_timeline(
  p_since timestamptz,
  p_until timestamptz,
  p_person_ids bigint[] default null,
  p_cohort_id bigint default null,
  p_kinds public.activity_timeline_kind[] default null,
  p_event_types public.event_type[] default null
) returns setof public.activity_timeline_item
language plpgsql
stable
as $$
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

comment on function public.activity_timeline(
  timestamptz,
  timestamptz,
  bigint[],
  bigint,
  public.activity_timeline_kind[],
  public.event_type[]
) is $$
@behavior +queryField:resource:list -queryField:resource:connection
$$;

grant all on function public.activity_timeline(
  timestamptz,
  timestamptz,
  bigint[],
  bigint,
  public.activity_timeline_kind[],
  public.event_type[]
) to anonymous;

do $$
begin
  if exists (
    select 1
    from information_schema.columns
    where table_schema = 'public'
      and table_name = 'person'
      and column_name = 'csts_id'
      and data_type <> 'integer'
  ) then
    alter table public.person
      alter column csts_id type integer
      using nullif(regexp_replace(csts_id::text, '\D', '', 'g'), '')::integer;
  end if;

  if exists (
    select 1
    from information_schema.columns
    where table_schema = 'public'
      and table_name = 'person'
      and column_name = 'wdsf_id'
      and data_type <> 'integer'
  ) then
    alter table public.person
      alter column wdsf_id type integer
      using nullif(regexp_replace(wdsf_id::text, '\D', '', 'g'), '')::integer;
  end if;

  if exists (
    select 1
    from information_schema.columns
    where table_schema = 'public'
      and table_name = 'membership_application'
      and column_name = 'csts_id'
      and data_type <> 'integer'
  ) then
    alter table public.membership_application
      alter column csts_id type integer
      using nullif(regexp_replace(csts_id::text, '\D', '', 'g'), '')::integer;
  end if;

  if exists (
    select 1
    from information_schema.columns
    where table_schema = 'public'
      and table_name = 'membership_application'
      and column_name = 'wdsf_id'
      and data_type <> 'integer'
  ) then
    alter table public.membership_application
      alter column wdsf_id type integer
      using nullif(regexp_replace(wdsf_id::text, '\D', '', 'g'), '')::integer;
  end if;
end
$$;

drop function if exists public.person_csts_candidates(public.person, integer);
drop function if exists public.person_csts_candidates(public.person, integer, real);

do $$
begin
  if exists (
    select 1
    from pg_attribute a
    join pg_class c on c.oid = a.attrelid
    join pg_namespace n on n.oid = c.relnamespace
    join pg_attrdef d on d.adrelid = a.attrelid and d.adnum = a.attnum
    join pg_depend dep on dep.objid = d.oid
    join pg_proc p on p.oid = dep.refobjid
    join pg_namespace pn on pn.oid = p.pronamespace
    where n.nspname = 'federated'
      and c.relname = 'person'
      and a.attname = 'search_name'
      and pn.nspname = 'federated'
      and p.proname = 'normalize_name'
  ) then
    alter table federated.person drop column search_name;
  end if;

  if exists (
    select 1
    from pg_attribute a
    join pg_class c on c.oid = a.attrelid
    join pg_namespace n on n.oid = c.relnamespace
    join pg_attrdef d on d.adrelid = a.attrelid and d.adnum = a.attnum
    join pg_depend dep on dep.objid = d.oid
    join pg_proc p on p.oid = dep.refobjid
    join pg_namespace pn on pn.oid = p.pronamespace
    where n.nspname = 'public'
      and c.relname = 'person'
      and a.attname = 'search_name'
      and pn.nspname = 'federated'
      and p.proname = 'normalize_name'
  ) then
    alter table public.person drop column search_name;
  end if;
end
$$;

alter table federated.person
  add column if not exists search_name text generated always as (
    app_private.normalize_name(coalesce(canonical_name, public.immutable_concat_ws(' ', first_name, last_name)))
  ) stored;

create index if not exists idx_person_search_name_trgm
  on federated.person using gin (search_name public.gin_trgm_ops);

alter table public.person
  add column if not exists search_name text generated always as (
    app_private.normalize_name(public.immutable_concat_ws(' ', first_name, last_name))
  ) stored;

comment on column public.person.search_name is '@omit';

create index if not exists federated_person_csts_search_name_trgm_idx
  on federated.person using gin (search_name public.gin_trgm_ops)
  where federation = 'csts';

create index if not exists person_csts_id_idx
  on public.person (csts_id)
  where csts_id is not null;

drop function if exists federated.normalize_name(text);

create or replace function public.person_csts_progress(in_person public.person) returns table (
  competitor_name text,
  category federated.category,
  points numeric(10, 3),
  finals integer
) as $$
select
  competitor.name as competitor_name,
  row(category.*) as category,
  ccp.points,
  ccp.domestic_finals + ccp.foreign_finals as finals
from federated.person p
       join federated.competitor_component cp on cp.person_id = p.id
       join federated.competitor on competitor.id = cp.competitor_id
       join federated.competitor_category_progress ccp on competitor.id = ccp.competitor_id
       join federated.category on ccp.category_id = category.id
where p.federation = 'csts'
  and p.external_id = in_person.csts_id::bigint;
$$ language sql stable;

comment on function public.person_csts_progress is '@simpleCollections only';
grant all on function public.person_csts_progress to anonymous;

create or replace function public.person_csts_candidates(in_person public.person, "limit" integer default 10, threshold real default 0.4) returns table (
  id integer,
  name text,
  age_group text,
  similarity real
) as $$
  with params as (
    select
      greatest(0.3::real, least(coalesce(threshold, 0.4), 1::real)) as effective_threshold,
      greatest(0, least(coalesce($2, 10), 50)) as effective_limit
  ),
  scored_candidates as (
    select
      fp.external_id::integer as id,
      fp.canonical_name as name,
      fp.age_group,
      score.name_score,
      case
        when in_person.birth_date is not null and fp.dob is not null and fp.dob = in_person.birth_date then 1
        else 0
      end as dob_score
      -- Future year-of-birth range scoring belongs here once federated.person.yob_range exists.
    from params
    join federated.person fp on true
    cross join lateral (
      select public.similarity(fp.search_name, in_person.search_name) as name_score
    ) score
    where in_person.search_name is not null
      and fp.federation = 'csts'
      and fp.external_id between 0 and 2147483647
      and fp.search_name % in_person.search_name
      and score.name_score >= params.effective_threshold
      and not exists (
        select 1
        from public.person existing
        where existing.id <> in_person.id
          and existing.csts_id = fp.external_id::integer
      )
  )
  select scored_candidates.id, scored_candidates.name, scored_candidates.age_group, scored_candidates.name_score
  from scored_candidates
  order by
    scored_candidates.dob_score desc,
    scored_candidates.name_score desc,
    scored_candidates.name,
    scored_candidates.id
  limit (select effective_limit from params);
$$ language sql stable;

comment on function public.person_csts_candidates is '@simpleCollections only';
grant all on function public.person_csts_candidates to anonymous;

create or replace function public.competition_brief(p_since date default null, p_until date default null, p_cohort_id bigint default null, p_person_ids bigint[] default null) returns setof public.competition_participation_record
    language sql stable
    as $$
  with params as (
    select
      coalesce(p_since, date_trunc('week', now())::date + 5) as since,
      coalesce(p_until, date_trunc('week', now())::date + 7) as until
  ),
  scoped_people as (
    select distinct p.id, p.name, p.csts_id, p.wdsf_id
    from public.current_tenant_membership tm
    join public.person p on p.id = tm.person_id
    where (p_person_ids is null or p.id = any(p_person_ids))
      and (
        p_cohort_id is null
        or exists (
          select 1
          from public.current_cohort_membership cm
          where cm.person_id = p.id
            and cm.cohort_id = p_cohort_id
        )
      )
  ),
  federated_people as (
    select
      sp.id as person_id,
      sp.name as person_name,
      fp.id as federated_person_id,
      fp.federation
    from scoped_people sp
    cross join lateral (
      values
        ('csts'::text, sp.csts_id::bigint),
        ('wdsf'::text, sp.wdsf_id::bigint)
    ) ids(federation, external_id)
    join federated.person fp
      on fp.federation = ids.federation
     and fp.external_id = ids.external_id
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
    comp.competition_type
  from params
  join federated.competition comp
    on comp.start_date >= params.since
   and comp.start_date < params.until
  join federated.event e on e.id = comp.event_id
  join federated.category cat on cat.id = comp.category_id
  join federated.competition_entry ce
    on ce.competition_id = comp.id
   and not ce.cancelled
  join federated.competitor c on c.id = ce.competitor_id
  join federated.competitor_component cc on cc.competitor_id = c.id
  join federated_people fp on fp.federated_person_id = cc.person_id
  left join lateral (
    select coalesce(array_agg(d.name order by dpd.dance_order), '{}'::text[]) as dances
    from federated.dance_program_dance dpd
    join federated.dance d on d.code = dpd.dance_code
    where dpd.program_id = cat.base_dance_program_id
  ) dances on true
  order by
    comp.start_date,
    comp.check_in_end nulls last,
    fp.person_name,
    cat.discipline,
    cat.class,
    c.name;
$$;

comment on function public.competition_brief(p_since date, p_until date, p_cohort_id bigint, p_person_ids bigint[]) is '@simpleCollections only';
grant all on function public.competition_brief(p_since date, p_until date, p_cohort_id bigint, p_person_ids bigint[]) to anonymous;

create or replace function public.competition_report(p_since date default null, p_until date default null, p_cohort_id bigint default null, p_person_ids bigint[] default null) returns setof public.competition_participation_record
    language sql stable
    as $$
  with params as (
    select
      coalesce(p_since, date_trunc('week', now())::date - 2) as since,
      coalesce(p_until, date_trunc('week', now())::date) as until
  ),
  scoped_people as (
    select distinct p.id, p.name, p.csts_id, p.wdsf_id
    from public.current_tenant_membership tm
    join public.person p on p.id = tm.person_id
    where (p_person_ids is null or p.id = any(p_person_ids))
      and (
        p_cohort_id is null
        or exists (
          select 1
          from public.current_cohort_membership cm
          where cm.person_id = p.id
            and cm.cohort_id = p_cohort_id
        )
      )
  ),
  federated_people as (
    select
      sp.id as person_id,
      sp.name as person_name,
      fp.id as federated_person_id,
      fp.federation
    from scoped_people sp
    cross join lateral (
      values
        ('csts'::text, sp.csts_id::bigint),
        ('wdsf'::text, sp.wdsf_id::bigint)
    ) ids(federation, external_id)
    join federated.person fp
      on fp.federation = ids.federation
     and fp.external_id = ids.external_id
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
    cr.ranking,
    cr.ranking_to,
    cr.point_gain,
    cr.is_final,
    true as has_result,
    comp.competition_type
  from params
  join federated.competition comp
    on comp.start_date >= params.since
   and comp.start_date < params.until
  join federated.event e on e.id = comp.event_id
  join federated.category cat on cat.id = comp.category_id
  join federated.competition_result cr on cr.competition_id = comp.id
  join federated.competitor c on c.id = cr.competitor_id
  join federated.competitor_component cc on cc.competitor_id = c.id
  join federated_people fp on fp.federated_person_id = cc.person_id
  left join lateral (
    select coalesce(array_agg(d.name order by dpd.dance_order), '{}'::text[]) as dances
    from federated.dance_program_dance dpd
    join federated.dance d on d.code = dpd.dance_code
    where dpd.program_id = cat.base_dance_program_id
  ) dances on true
  order by
    comp.start_date,
    fp.person_name,
    cat.discipline,
    cat.class,
    cr.ranking,
    c.name;
$$;

comment on function public.competition_report(p_since date, p_until date, p_cohort_id bigint, p_person_ids bigint[]) is '@simpleCollections only';
grant all on function public.competition_report(p_since date, p_until date, p_cohort_id bigint, p_person_ids bigint[]) to anonymous;

select verify_function('public.activity_timeline');
