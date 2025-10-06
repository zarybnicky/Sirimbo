--! Previous: sha1:a62e79890f2060636af8b38d578a46151ce5d5dd

-- Rework upsert_announcement audience handling to honor delete/insert/update semantics
DROP FUNCTION IF EXISTS upsert_announcement;
drop type if exists announcement_cohort_type_input;
drop type if exists announcement_audience_type_input;

do $$
begin
  if not exists (
    select 1
    from pg_type t
    join pg_namespace n on n.oid = t.typnamespace
    where t.typname = 'announcement_audience_type_input'
      and n.nspname = 'public'
  ) then
    create type announcement_audience_type_input as (
      id bigint,
      cohort_id bigint,
      audience_role announcement_audience_role
    );
  end if;
end;
$$;

create or replace function upsert_announcement(
  info announcement_type_input,
  audiences announcement_audience_type_input[] default null
) returns announcement
language plpgsql
as $$
declare
  v_announcement announcement;
begin
  if info.id is not null then
    update announcement set
      title = info.title,
      body = info.body,
      is_locked = coalesce(info.is_locked, false),
      is_visible = coalesce(info.is_visible, true),
      is_sticky = coalesce(info.is_sticky, false),
      scheduled_since = info.scheduled_since,
      scheduled_until = info.scheduled_until
    where id = info.id
    returning * into v_announcement;

    if not found then
      raise exception 'Announcement with id % not found', info.id;
    end if;
  else
    insert into announcement (
      title,
      body,
      is_locked,
      is_visible,
      is_sticky,
      scheduled_since,
      scheduled_until
    )
    values (
      info.title,
      info.body,
      coalesce(info.is_locked, false),
      coalesce(info.is_visible, true),
      coalesce(info.is_sticky, false),
      info.scheduled_since,
      info.scheduled_until
    )
    returning * into v_announcement;
  end if;

  if audiences is not null then
    with audience_input as (
      select distinct
        (a).id as id,
        (a).cohort_id as cohort_id,
        (a).audience_role as audience_role
      from unnest(audiences) a
    )
    delete from announcement_audience aa
    using audience_input ai
    where aa.announcement_id = v_announcement.id
      and aa.id = ai.id
      and ai.id is not null
      and ai.cohort_id is null
      and ai.audience_role is null;

    with audience_input as (
      select distinct
        (a).id as id,
        (a).cohort_id as cohort_id,
        (a).audience_role as audience_role
      from unnest(audiences) a
    )
    update announcement_audience aa
    set cohort_id = ai.cohort_id,
        audience_role = ai.audience_role
    from audience_input ai
    where aa.announcement_id = v_announcement.id
      and aa.id = ai.id
      and ai.id is not null
      and ((ai.cohort_id is not null and ai.audience_role is null) or (ai.cohort_id is null and ai.audience_role is not null))
      and (
        aa.cohort_id is distinct from ai.cohort_id or
        aa.audience_role is distinct from ai.audience_role
      );

    with audience_input as (
      select distinct
        (a).cohort_id as cohort_id
      from unnest(audiences) a
      where (a).id is null
        and (a).cohort_id is not null
        and (a).audience_role is null
    )
    insert into announcement_audience (announcement_id, cohort_id)
    select v_announcement.id, ai.cohort_id
    from audience_input ai
    on conflict (announcement_id, cohort_id) do nothing;

    with audience_input as (
      select distinct
        (a).audience_role as audience_role
      from unnest(audiences) a
      where (a).id is null
        and (a).cohort_id is null
        and (a).audience_role is not null
    )
    insert into announcement_audience (announcement_id, audience_role)
    select v_announcement.id, ai.audience_role
    from audience_input ai
    on conflict (announcement_id, audience_role) do nothing;
  end if;

  return v_announcement;
end;
$$;
select verify_function('upsert_announcement');
grant all on function upsert_announcement to anonymous;

create or replace function sticky_announcements(
  order_by_updated boolean default false
) returns setof announcement
language sql stable
as $$
  with audience_claims as (
    select
      translate(coalesce(nullif(current_setting('jwt.claims.my_cohort_ids', true), ''), '[]'), '[]', '{}')::bigint[] as cohort_ids,
      coalesce(nullif(current_setting('jwt.claims.is_member', true), '')::boolean, false) as is_member,
      coalesce(nullif(current_setting('jwt.claims.is_trainer', true), '')::boolean, false) as is_trainer,
      coalesce(nullif(current_setting('jwt.claims.is_admin', true), '')::boolean, false) as is_admin
  )
  select announcement.*
  from announcement
  cross join audience_claims ac
  where is_visible = true
    and is_sticky = true
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
    and (
      not exists (
        select 1
        from announcement_audience aa_all
        where aa_all.announcement_id = announcement.id
      )
      or exists (
        select 1
        from announcement_audience aa
        where aa.announcement_id = announcement.id
          and (
            (aa.cohort_id is not null and aa.cohort_id = any (ac.cohort_ids))
            or (aa.audience_role = 'member' and ac.is_member)
            or (aa.audience_role = 'trainer' and ac.is_trainer)
            or (aa.audience_role = 'administrator' and ac.is_admin)
          )
      )
    )
  order by
    case when order_by_updated then updated_at else created_at end desc,
    created_at desc;
$$;
grant all on function sticky_announcements(order_by_updated boolean) to anonymous;

create or replace function my_announcements(
  archive boolean default false,
  order_by_updated boolean default false
) returns setof announcement
language sql stable
as $$
  with audience_claims as (
    select
      translate(coalesce(nullif(current_setting('jwt.claims.my_cohort_ids', true), ''), '[]'), '[]', '{}')::bigint[] as cohort_ids,
      coalesce(nullif(current_setting('jwt.claims.is_member', true), '')::boolean, false) as is_member,
      coalesce(nullif(current_setting('jwt.claims.is_trainer', true), '')::boolean, false) as is_trainer,
      coalesce(nullif(current_setting('jwt.claims.is_admin', true), '')::boolean, false) as is_admin
  )
  select announcement.*
  from announcement
  cross join audience_claims ac
  where is_visible = not archive
    and is_sticky = false
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
    and (
      not exists (
        select 1
        from announcement_audience aa_all
        where aa_all.announcement_id = announcement.id
      )
      or exists (
        select 1
        from announcement_audience aa
        where aa.announcement_id = announcement.id
          and (
            (aa.cohort_id is not null and aa.cohort_id = any (ac.cohort_ids))
            or (aa.audience_role = 'member' and ac.is_member)
            or (aa.audience_role = 'trainer' and ac.is_trainer)
            or (aa.audience_role = 'administrator' and ac.is_admin)
          )
      )
    )
  order by
    case when order_by_updated then updated_at else created_at end desc,
    created_at desc;
$$;
grant all on function my_announcements(archive boolean, order_by_updated boolean) to anonymous;
