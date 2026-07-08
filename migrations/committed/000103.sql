--! Previous: sha1:d707b4e496e707f569195e6ee1d76902bf8a1184
--! Hash: sha1:5642da1a74ba3d65b625e2757dcb67ac418a757c

--! split: 1-current.sql
alter table person
  add column if not exists instagram_username text,
  add column if not exists tiktok_username text,
  add column if not exists facebook_url text,
  add column if not exists website_url text;

--! Included functions/create_person.sql
drop function if exists create_person;

CREATE or replace FUNCTION public.create_person(person_id bigint, INOUT p public.person, is_member boolean, is_trainer boolean, is_admin boolean, send_invitation boolean, join_date timestamp with time zone) RETURNS public.person
    LANGUAGE plpgsql
    AS $$
begin
  if person_id is null then
    insert into person (
      first_name,
      last_name,
      gender,
      birth_date,
      nationality,
      tax_identification_number,
      national_id_number,
      csts_id,
      wdsf_id,
      prefix_title,
      suffix_title,
      bio,
      email,
      phone,
      external_ids,
      instagram_username,
      tiktok_username,
      facebook_url,
      website_url
    ) values (
      p.first_name,
      p.last_name,
      p.gender,
      p.birth_date,
      p.nationality,
      p.tax_identification_number,
      p.national_id_number,
      p.csts_id,
      p.wdsf_id,
      p.prefix_title,
      p.suffix_title,
      p.bio,
      p.email,
      p.phone,
      p.external_ids,
      p.instagram_username,
      p.tiktok_username,
      p.facebook_url,
      p.website_url
    ) returning * into p;
  else
    select * into p from person where person.id=person_id;
  end if;
  if is_member = true then
    insert into tenant_membership (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if is_trainer = true then
    insert into tenant_trainer (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if is_admin = true then
    insert into tenant_administrator (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if send_invitation = true and p.email is not null and p.email <> '' then
    insert into person_invitation (person_id, tenant_id, email) values (p.id, current_tenant_id(), p.email);
  end if;
end
$$;
select verify_function('create_person');
grant all on function create_person to anonymous;
--! EndIncluded functions/create_person.sql

do $$
begin
  if not exists (
    select 1
    from pg_catalog.pg_enum e
    join pg_catalog.pg_type t on t.oid = e.enumtypid
    join pg_catalog.pg_namespace n on n.oid = t.typnamespace
    where n.nspname = 'public'
      and t.typname = 'activity_timeline_kind'
      and e.enumlabel = 'JUDGING'
  ) or not exists (
    select 1
    from pg_catalog.pg_enum e
    join pg_catalog.pg_type t on t.oid = e.enumtypid
    join pg_catalog.pg_namespace n on n.oid = t.typnamespace
    where n.nspname = 'public'
      and t.typname = 'activity_timeline_kind'
      and e.enumlabel = 'BIRTHDAY'
  ) then
    drop function if exists activity_timeline;
    drop view if exists activity_timeline_item;
    alter type activity_timeline_kind rename to activity_timeline_kind_old;
    create type activity_timeline_kind as enum (
      'EVENT_ATTENDANCE',
      'COMPETITION_BRIEF',
      'COMPETITION_RESULT',
      'JUDGING',
      'BIRTHDAY'
    );
    drop type activity_timeline_kind_old;
  end if;
end;
$$;

create or replace view activity_timeline_item as
select
  null::text as id,
  null::activity_timeline_kind as kind,
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

comment on view activity_timeline_item is $$
@primaryKey id
@interface mode:single type:kind
@type EVENT_ATTENDANCE name:ActivityEventAttendance attributes:event_attendance_id,event_instance_id
@type COMPETITION_BRIEF name:ActivityCompetitionBrief attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,check_in_end,category,dances,participants,competition_type,competition_event_external_id,competition_external_id
@type COMPETITION_RESULT name:ActivityCompetitionResult attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,category,dances,participants,ranking,ranking_to,point_gain,is_final,competition_type,competition_event_external_id,competition_external_id
@type JUDGING name:ActivityJudging attributes:federation,federated_person_id,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,category,competition_type,competition_event_external_id,competition_external_id
@type BIRTHDAY name:ActivityBirthday
@foreignKey (person_id) references person (id)|@fieldName person|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_attendance_id) references event_attendance (id)|@fieldName eventAttendance|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_instance_id) references event_instance (id)|@fieldName eventInstance|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@behavior -query:resource:list -query:resource:connection -query:resource:single
$$;

grant select on activity_timeline_item to anonymous;

create or replace function activity_timeline(
  p_since timestamptz,
  p_until timestamptz,
  p_person_ids bigint[] default null,
  p_cohort_id bigint default null,
  p_kinds activity_timeline_kind[] default null,
  p_event_types event_type[] default null
) returns setof activity_timeline_item
language plpgsql stable
as $$
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
        select distinct p.id
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
        null::federated.competition_type as competition_type,
        null::text as competition_event_external_id,
        null::text as competition_external_id
      from event_attendance ea
      join event_instance ei on ei.id = ea.instance_id
      join person p on p.id = ea.person_id
      join scoped_people sp on sp.id = ea.person_id
      where ea.status <> 'cancelled'
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
$$;

comment on function activity_timeline is '@behavior +queryField:resource:list -queryField:resource:connection';
grant all on function activity_timeline to anonymous;
select verify_function('activity_timeline');

alter table federated.competition_official
  add column if not exists external_id text;

create index if not exists competition_official_competition_id_external_id_idx
  on federated.competition_official (competition_id, external_id)
  where external_id is not null;

alter table federated.competition_result
  drop constraint if exists competition_result_competition_id_start_number_key;

--! split: 2-jwt-token.sql
DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_attribute a JOIN pg_type t ON t.typrelid = a.attrelid
    WHERE t.oid = to_regtype('public.jwt_token') AND a.attname = 'guest_tenant_ids'
  ) THEN
    ALTER TYPE public.jwt_token ADD ATTRIBUTE guest_tenant_ids bigint[];
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM pg_attribute a JOIN pg_type t ON t.typrelid = a.attrelid
    WHERE t.oid = to_regtype('public.jwt_token') AND a.attname = 'member_tenant_ids'
  ) THEN
    ALTER TYPE public.jwt_token ADD ATTRIBUTE member_tenant_ids bigint[];
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM pg_attribute a JOIN pg_type t ON t.typrelid = a.attrelid
    WHERE t.oid = to_regtype('public.jwt_token') AND a.attname = 'trainer_tenant_ids'
  ) THEN
    ALTER TYPE public.jwt_token ADD ATTRIBUTE trainer_tenant_ids bigint[];
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM pg_attribute a JOIN pg_type t ON t.typrelid = a.attrelid
    WHERE t.oid = to_regtype('public.jwt_token') AND a.attname = 'admin_tenant_ids'
  ) THEN
    ALTER TYPE public.jwt_token ADD ATTRIBUTE admin_tenant_ids bigint[];
  END IF;
END
$$ LANGUAGE plpgsql;

--! Included functions/create_jwt_token.sql
create or replace function app_private.create_jwt_token(u users) returns jwt_token
    language sql stable
as $$
with
  person_ids as (
    select distinct up.person_id from user_proxy up where up.user_id = u.id
  ),
  tenant_memberships as (
    select distinct tm.tenant_id from tenant_membership tm join person_ids p on p.person_id = tm.person_id where tm.status = 'active'
  ),
  tenant_trainers as (
    select distinct tt.tenant_id from tenant_trainer tt join person_ids p on p.person_id = tt.person_id where tt.status = 'active'
  ),
  tenant_admins as (
    select distinct ta.tenant_id from tenant_administrator ta join person_ids p on p.person_id = ta.person_id where ta.status = 'active'
    union
    select id from tenant where app_private.is_system_admin(u.id)
  ),
  tenant_ids as (
    select tenant_id from tenant_memberships
    union
    select tenant_id from tenant_trainers
    union
    select tenant_id from tenant_admins
  ),
  cohort_ids as (
    select distinct cm.cohort_id from cohort_membership cm join person_ids p on p.person_id = cm.person_id where cm.status = 'active'
  ),
  couple_ids as (
    select distinct c.id from couple c join person_ids p on p.person_id = c.man_id where c.status = 'active'
    union all
    select distinct c.id from couple c join person_ids p on p.person_id = c.woman_id where c.status = 'active'
  )
  select
    extract(epoch from now() + interval '7 days')::integer as exp,
    u.id as user_id,
    (select current_tenant_id()) as tenant_id,
    u.u_login as username,
    u.u_email as email,

    coalesce((select array_agg(p.person_id) from person_ids p), '{}'::bigint[]) as my_person_ids,
    coalesce((select array_agg(p.tenant_id) from tenant_ids p), '{}'::bigint[]) as my_tenant_ids,
    coalesce((select array_agg(p.cohort_id) from cohort_ids p), '{}'::bigint[]) as my_cohort_ids,
    coalesce((select array_agg(p.id) from couple_ids p), '{}'::bigint[]) as my_couple_ids,

    exists (select 1 from tenant_ids t where t.tenant_id = (select current_tenant_id())) as is_member,
    exists (select 1 from tenant_trainers t where t.tenant_id = (select current_tenant_id())) as is_trainer,
    exists (select 1 from tenant_admins a where a.tenant_id = (select current_tenant_id())) as is_admin,
    app_private.is_system_admin(u.id) as is_system_admin,

    '{}'::bigint[] as guest_tenant_ids,
    coalesce((select array_agg(p.tenant_id) from tenant_memberships p), '{}'::bigint[]) as member_tenant_ids,
    coalesce((select array_agg(p.tenant_id) from tenant_trainers p), '{}'::bigint[]) as trainer_tenant_ids,
    coalesce((select array_agg(p.tenant_id) from tenant_admins p), '{}'::bigint[]) as admin_tenant_ids;
$$;
--! EndIncluded functions/create_jwt_token.sql

--! split: 3-event-refactor.sql
do $$
begin
  if not exists (select * from pg_type where typcategory='E' and typname = 'event_capacity_unit') then
    create type event_capacity_unit as enum (
      'people',
      'registrations'
    );
  end if;
end
$$;

alter table event_instance
  add column if not exists parent_id bigint,
  add column if not exists capacity integer,
  add column if not exists capacity_unit event_capacity_unit default 'registrations',
  add column if not exists description text,
  add column if not exists summary text,
  add column if not exists is_locked boolean,
  add column if not exists enable_notes boolean,
  add column if not exists files_legacy text,
  alter column event_id drop not null;

do $$
begin
  if not exists (
    select 1 from pg_constraint where conname = 'event_instance_parent_id_fkey'
  ) then
    alter table event_instance
      add constraint event_instance_parent_id_fkey
      foreign key (parent_id) references event_instance (id)
      on update cascade on delete cascade;
  end if;
end;
$$;

create index if not exists event_instance_parent_id_idx
  on event_instance (parent_id);

drop function if exists event_instances_for_range;

--! Included functions/event_instances_for_range.sql
CREATE or replace FUNCTION public.event_instances_for_range(
  only_type public.event_type,
  start_range timestamp with time zone,
  end_range timestamp with time zone DEFAULT NULL::timestamp with time zone,
  trainer_ids bigint[] = null,
  participant_ids bigint[] = null,
  only_mine boolean = false,
  parent_id bigint = null
) RETURNS SETOF event_instance as $$
  select i.*
  from event_instance i
  where i.tenant_id = current_tenant_id()
    and (only_type IS NULL OR i.type = only_type)
    and (parent_id is null or i.parent_id = parent_id)
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
--! Included functions/can_trainer_edit_instance.sql
create or replace function app_private.can_trainer_edit_instance(iid bigint)
  returns boolean
  language sql stable security definer leakproof parallel safe
as $$
  with recursive chain as (
    select i.id, i.parent_id, i.event_id from event_instance i where i.id = iid
    union all
    select p.id, p.parent_id, p.event_id from event_instance p join chain c on p.id = c.parent_id
  ),
  trainers as (
    select person_id from event_instance_trainer where instance_id in (select id from chain)
    union
    select person_id from event_trainer where event_id in (select event_id from chain where event_id is not null)
  )
  select exists (
    select 1 from trainers where person_id = any ((select current_person_ids())::bigint[])
  ) or not exists (
    select 1 from trainers
  );
$$;

grant all on function app_private.can_trainer_edit_instance(bigint) to anonymous;
--! EndIncluded functions/can_trainer_edit_instance.sql

do $$
begin
  if not exists (select 1 from pg_constraint where conname = 'event_instance_tenant_id_id_key') then
    alter table event_instance
      add constraint event_instance_tenant_id_id_key unique (tenant_id, id);
  end if;
end
$$;

alter table event_instance_trainer
  drop constraint if exists event_instance_trainer_tenant_id_instance_id_event_id_fkey;
do $$
begin
  if not exists (select 1 from pg_constraint where conname = 'event_instance_trainer_tenant_id_instance_id_fkey') then
    alter table event_instance_trainer
      add constraint event_instance_trainer_tenant_id_instance_id_fkey
      foreign key (tenant_id, instance_id) references event_instance (tenant_id, id)
      on update cascade on delete cascade;
  end if;
end
$$;
alter table event_instance_trainer alter column event_id drop not null;

comment on constraint event_instance_trainer_tenant_id_instance_id_fkey
  on public.event_instance_trainer
  is E'@fieldName instance
@foreignFieldName eventInstanceTrainersByInstanceId';

alter table event_attendance
  drop constraint if exists event_attendance_tenant_id_instance_id_event_id_fkey;
do $$
begin
  if not exists (select 1 from pg_constraint where conname = 'event_attendance_tenant_id_instance_id_fkey') then
    alter table event_attendance
      add constraint event_attendance_tenant_id_instance_id_fkey
      foreign key (tenant_id, instance_id) references event_instance (tenant_id, id)
      on update cascade on delete cascade;
  end if;
end
$$;
alter table event_attendance alter column event_id drop not null;

comment on constraint event_attendance_tenant_id_instance_id_fkey
  on public.event_attendance
  is E'@fieldName instance
@foreignFieldName eventAttendancesByInstanceId';

select app_private.drop_policies('public.event_instance');
create policy current_tenant on event_instance as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_same_tenant on event_instance to administrator using (true);
create policy trainer_same_tenant on event_instance to trainer using (app_private.can_trainer_edit_instance(id)) with check (true);
create policy member_view on event_instance for select to member using (is_visible);
create policy public_view on event_instance for select to anonymous using (is_public);
grant all on table event_instance to anonymous;

select app_private.drop_policies('public.event_instance_trainer');
create policy current_tenant on event_instance_trainer as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_instance_trainer to administrator using (true);
create policy trainer_same_tenant on event_instance_trainer to trainer
  using (app_private.can_trainer_edit_instance(instance_id)) with check (true);
create policy member_view on event_instance_trainer for select to member using (true);
grant all on table event_instance_trainer to anonymous;


create or replace function public.tg_event_instance__fill_defaults()
  returns trigger
  language plpgsql
as $$
declare e public.event;
begin
  -- standalone instance: no event to inherit from, its own values stand
  if new.event_id is null then
    new.custom := coalesce(new.custom, '{}'::jsonb);
    return new;
  end if;

  select * into strict e
  from public.event
  where tenant_id = new.tenant_id and id = new.event_id;

  new.custom := coalesce(new.custom, '{}'::jsonb);

  if new.name          is null then new.name          := e.name;          else new.custom := new.custom || jsonb_build_object('name', true);          end if;
  if new.type          is null then new.type          := e.type;          else new.custom := new.custom || jsonb_build_object('type', true);          end if;
  if new.location_text is null then new.location_text := e.location_text; else new.custom := new.custom || jsonb_build_object('location_text', true); end if;
  if new.location_id   is null then new.location_id   := e.location_id;   else new.custom := new.custom || jsonb_build_object('location_id', true);   end if;
  if new.is_visible    is null then new.is_visible    := e.is_visible;    else new.custom := new.custom || jsonb_build_object('is_visible', true);    end if;
  if new.is_public     is null then new.is_public     := e.is_public;     else new.custom := new.custom || jsonb_build_object('is_public', true);     end if;
  if new.capacity      is null then new.capacity      := e.capacity;      else new.custom := new.custom || jsonb_build_object('capacity', true);      end if;
  if new.is_locked     is null then new.is_locked     := e.is_locked;     else new.custom := new.custom || jsonb_build_object('is_locked', true);     end if;
  if new.description   is null then new.description   := e.description;   else new.custom := new.custom || jsonb_build_object('description', true);   end if;
  if new.summary       is null then new.summary       := e.summary;       else new.custom := new.custom || jsonb_build_object('summary', true);       end if;
  if new.enable_notes  is null then new.enable_notes  := e.enable_notes;  else new.custom := new.custom || jsonb_build_object('enable_notes', true);  end if;
  if new.files_legacy  is null then new.files_legacy  := e.files_legacy;  else new.custom := new.custom || jsonb_build_object('files_legacy', true);  end if;

  return new;
end;
$$;
select verify_function('tg_event_instance__fill_defaults', 'event_instance');

create or replace function public.tg_event_instance__pin_overrides()
  returns trigger
  language plpgsql
as $$
declare
  e public.event;
  c jsonb;
begin
  c := coalesce(new.custom, '{}'::jsonb);

  -- standalone instance: nothing to inherit, so no override flags to maintain
  if new.event_id is null then
    new.custom := c;
    return new;
  end if;

  if new.event_id is distinct from old.event_id then
    select * into strict e
    from public.event
    where tenant_id = new.tenant_id and id = new.event_id;

    if not (c ? 'name')          then new.name          := e.name;          end if;
    if not (c ? 'type')          then new.type          := e.type;          end if;
    if not (c ? 'location_text') then new.location_text := e.location_text; end if;
    if not (c ? 'location_id')   then new.location_id   := e.location_id;   end if;
    if not (c ? 'is_visible')    then new.is_visible    := e.is_visible;    end if;
    if not (c ? 'is_public')     then new.is_public     := e.is_public;     end if;
    if not (c ? 'capacity')      then new.capacity      := e.capacity;      end if;
    if not (c ? 'is_locked')     then new.is_locked     := e.is_locked;     end if;
    if not (c ? 'description')   then new.description   := e.description;   end if;
    if not (c ? 'summary')       then new.summary       := e.summary;       end if;
    if not (c ? 'enable_notes')  then new.enable_notes  := e.enable_notes;  end if;
    if not (c ? 'files_legacy')  then new.files_legacy  := e.files_legacy;  end if;

    new.custom := c;
    return new;
  end if;

  if (new.name          is distinct from old.name) or
     (new.type          is distinct from old.type) or
     (new.location_text is distinct from old.location_text) or
     (new.location_id   is distinct from old.location_id) or
     (new.is_visible    is distinct from old.is_visible) or
     (new.is_public     is distinct from old.is_public) or
     (new.capacity      is distinct from old.capacity) or
     (new.is_locked     is distinct from old.is_locked) or
     (new.description   is distinct from old.description) or
     (new.summary       is distinct from old.summary) or
     (new.enable_notes  is distinct from old.enable_notes) or
     (new.files_legacy  is distinct from old.files_legacy) or
     (new.custom        is distinct from old.custom)
  then
   select * into strict e
    from public.event
    where tenant_id = new.tenant_id and id = new.event_id;
  end if;

  if new.name is distinct from old.name then
    if new.name is not distinct from e.name then c := c - 'name'; else c := c || jsonb_build_object('name', true); end if;
  end if;
  if new.type is distinct from old.type then
    if new.type is not distinct from e.type then c := c - 'type'; else c := c || jsonb_build_object('type', true); end if;
  end if;
  if new.location_text is distinct from old.location_text then
    if new.location_text is not distinct from e.location_text then c := c - 'location_text'; else c := c || jsonb_build_object('location_text', true); end if;
  end if;
  if new.location_id is distinct from old.location_id then
    if new.location_id is not distinct from e.location_id then c := c - 'location_id'; else c := c || jsonb_build_object('location_id', true); end if;
  end if;
  if new.is_visible is distinct from old.is_visible then
    if new.is_visible is not distinct from e.is_visible then c := c - 'is_visible'; else c := c || jsonb_build_object('is_visible', true); end if;
  end if;
  if new.is_public is distinct from old.is_public then
    if new.is_public is not distinct from e.is_public then c := c - 'is_public'; else c := c || jsonb_build_object('is_public', true); end if;
  end if;
  if new.capacity is distinct from old.capacity then
    if new.capacity is not distinct from e.capacity then c := c - 'capacity'; else c := c || jsonb_build_object('capacity', true); end if;
  end if;
  if new.is_locked is distinct from old.is_locked then
    if new.is_locked is not distinct from e.is_locked then c := c - 'is_locked'; else c := c || jsonb_build_object('is_locked', true); end if;
  end if;
  if new.description is distinct from old.description then
    if new.description is not distinct from e.description then c := c - 'description'; else c := c || jsonb_build_object('description', true); end if;
  end if;
  if new.summary is distinct from old.summary then
    if new.summary is not distinct from e.summary then c := c - 'summary'; else c := c || jsonb_build_object('summary', true); end if;
  end if;
  if new.enable_notes is distinct from old.enable_notes then
    if new.enable_notes is not distinct from e.enable_notes then c := c - 'enable_notes'; else c := c || jsonb_build_object('enable_notes', true); end if;
  end if;
  if new.files_legacy is distinct from old.files_legacy then
    if new.files_legacy is not distinct from e.files_legacy then c := c - 'files_legacy'; else c := c || jsonb_build_object('files_legacy', true); end if;
  end if;

  new.custom := c;
  return new;
end;
$$;
select verify_function('tg_event_instance__pin_overrides', 'event_instance');

-- pin_overrides only fires on UPDATE OF its listed columns — extend the list.
drop trigger if exists _800_event_instance__pin_overrides on public.event_instance;
create trigger _800_event_instance__pin_overrides
  before update of event_id, name, type, location_text, location_id, is_visible,
    is_public, capacity, is_locked, description, summary, enable_notes, files_legacy, custom
  on public.event_instance
  for each row execute function public.tg_event_instance__pin_overrides();

create or replace function public.tg_event__propagate_to_instances()
  returns trigger
  language plpgsql
as $$
begin
  if (row(old.name, old.type, old.location_text, old.location_id, old.is_visible, old.is_public,
          old.capacity, old.is_locked, old.description, old.summary, old.enable_notes, old.files_legacy)
    is not distinct from
      row(new.name, new.type, new.location_text, new.location_id, new.is_visible, new.is_public,
          new.capacity, new.is_locked, new.description, new.summary, new.enable_notes, new.files_legacy)) then
    return null;
  end if;

  update event_instance ei
  set
    name          = case when (ei.custom ? 'name')          then ei.name          else new.name          end,
    type          = case when (ei.custom ? 'type')          then ei.type          else new.type          end,
    location_text = case when (ei.custom ? 'location_text') then ei.location_text else new.location_text end,
    location_id   = case when (ei.custom ? 'location_id')   then ei.location_id   else new.location_id   end,
    is_visible    = case when (ei.custom ? 'is_visible')    then ei.is_visible    else new.is_visible    end,
    is_public     = case when (ei.custom ? 'is_public')     then ei.is_public     else new.is_public     end,
    capacity      = case when (ei.custom ? 'capacity')      then ei.capacity      else new.capacity      end,
    is_locked     = case when (ei.custom ? 'is_locked')     then ei.is_locked     else new.is_locked     end,
    description   = case when (ei.custom ? 'description')   then ei.description   else new.description   end,
    summary       = case when (ei.custom ? 'summary')       then ei.summary       else new.summary       end,
    enable_notes  = case when (ei.custom ? 'enable_notes')  then ei.enable_notes  else new.enable_notes  end,
    files_legacy  = case when (ei.custom ? 'files_legacy')  then ei.files_legacy  else new.files_legacy  end
  where ei.tenant_id = new.tenant_id
    and ei.event_id = new.id
    and (
      ((not (ei.custom ? 'name'))          and ei.name          is distinct from new.name) or
      ((not (ei.custom ? 'type'))          and ei.type          is distinct from new.type) or
      ((not (ei.custom ? 'location_text')) and ei.location_text is distinct from new.location_text) or
      ((not (ei.custom ? 'location_id'))   and ei.location_id   is distinct from new.location_id) or
      ((not (ei.custom ? 'is_visible'))    and ei.is_visible    is distinct from new.is_visible) or
      ((not (ei.custom ? 'is_public'))     and ei.is_public     is distinct from new.is_public) or
      ((not (ei.custom ? 'capacity'))      and ei.capacity      is distinct from new.capacity) or
      ((not (ei.custom ? 'is_locked'))     and ei.is_locked     is distinct from new.is_locked) or
      ((not (ei.custom ? 'description'))   and ei.description   is distinct from new.description) or
      ((not (ei.custom ? 'summary'))       and ei.summary       is distinct from new.summary) or
      ((not (ei.custom ? 'enable_notes'))  and ei.enable_notes  is distinct from new.enable_notes) or
      ((not (ei.custom ? 'files_legacy'))  and ei.files_legacy  is distinct from new.files_legacy)
    );

  return null;
end;
$$;
select verify_function('tg_event__propagate_to_instances', 'event');

update public.event_instance ei
set capacity     = coalesce(ei.capacity, e.capacity),
    is_locked    = coalesce(ei.is_locked, e.is_locked),
    description  = coalesce(ei.description, e.description),
    summary      = coalesce(ei.summary, e.summary),
    enable_notes = coalesce(ei.enable_notes, e.enable_notes),
    files_legacy = coalesce(ei.files_legacy, e.files_legacy)
from public.event e
where ei.event_id = e.id
  and (ei.capacity is null or ei.is_locked is null or ei.description is null
    or ei.summary is null or ei.enable_notes is null or ei.files_legacy is null);
