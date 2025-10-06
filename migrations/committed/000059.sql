--! Previous: sha1:2e32f51a85842cab7f681c2cfdacd1bbaea9fddd
--! Hash: sha1:7dfb5c476b3c145cb90470579d8488b9590c25c1

--! split: 1-current.sql
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


create or replace function app_private.queue_announcement_notifications(in_announcement_id bigint)
returns void
language plpgsql
security definer
as $$
declare
  v_tenant_id bigint;
  v_is_visible boolean;
  v_since timestamptz;
  v_until timestamptz;
  v_user_ids bigint[];
begin
  select tenant_id,
         coalesce(is_visible, false),
         scheduled_since,
         scheduled_until
  into v_tenant_id,
    v_is_visible,
    v_since,
    v_until
  from announcement
  where id = in_announcement_id;

  if not found then
    return;
  end if;

  if not (v_is_visible
      and (v_since is null or v_since <= now())
      and (v_until is null or v_until > now())) then
    return;
  end if;

  with role_flags as (
    select
      coalesce(bool_or(audience_role = 'member'), false) as has_member,
      coalesce(bool_or(audience_role = 'trainer'), false) as has_trainer,
      coalesce(bool_or(audience_role = 'administrator'), false) as has_administrator
    from announcement_audience
    where announcement_id = in_announcement_id
      and audience_role is not null
  ),
  role_people as (
    select distinct ad.person_id
    from auth_details ad
    join role_flags rf on rf.has_member or rf.has_trainer or rf.has_administrator
    where (
      rf.has_member and v_tenant_id = any (coalesce(ad.tenant_memberships, '{}'::bigint[]))
    ) or (
      rf.has_trainer and v_tenant_id = any (coalesce(ad.tenant_trainers, '{}'::bigint[]))
    ) or (
      rf.has_administrator and v_tenant_id = any (coalesce(ad.tenant_administrators, '{}'::bigint[]))
    )
  ),
  role_users as (
    select distinct u.u_id as user_id
    from role_people rp
    join user_proxy up on up.person_id = rp.person_id and up.active
    join users u on u.u_id = up.user_id
    where u.tenant_id = v_tenant_id
  ),
  cohort_users as (
    select distinct u.u_id as user_id
    from announcement_audience aa
    join cohort_membership cm
      on cm.cohort_id = aa.cohort_id
     and cm.active
    join user_proxy up on up.person_id = cm.person_id and up.active
    join users u on u.u_id = up.user_id
    where aa.announcement_id = in_announcement_id
      and aa.cohort_id is not null
      and aa.tenant_id = v_tenant_id
      and cm.tenant_id = v_tenant_id
      and u.tenant_id = v_tenant_id
  )
  select array_agg(distinct user_id order by user_id)
  into v_user_ids
  from (
    select user_id from role_users
    union
    select user_id from cohort_users
  ) recipients;

  if v_user_ids is null or array_length(v_user_ids, 1) = 0 then
    return;
  end if;

  perform graphile_worker.add_job(
    'notify_announcement',
    json_build_object(
      'announcement_id', in_announcement_id,
      'user_ids', v_user_ids
    )
  );
end;
$$;
select verify_function('app_private.queue_announcement_notifications');
comment on function app_private.queue_announcement_notifications is '@omit';
grant all on function app_private.queue_announcement_notifications to anonymous;

create or replace function app_private.tg_announcement__after_write()
returns trigger
language plpgsql
security definer
as $$
-- @plpgsql_check_options: oldtable = oldtable, newtable = newtable
declare
  rec record;
  old_row record;
  was_published boolean;
  is_published boolean;
begin
  for rec in
    select * from newtable
  loop
    is_published := coalesce(rec.is_visible, false)
      and (rec.scheduled_since is null or rec.scheduled_since <= now())
      and (rec.scheduled_until is null or rec.scheduled_until > now());

    if TG_OP = 'INSERT' then
      was_published := false;
    else
      select * into old_row
      from oldtable
      where id = rec.id;

      if not found then
        was_published := false;
      else
        was_published := coalesce(old_row.is_visible, false)
          and (old_row.scheduled_since is null or old_row.scheduled_since <= now())
          and (old_row.scheduled_until is null or old_row.scheduled_until > now());
      end if;
    end if;

    if is_published and not was_published then
      perform app_private.queue_announcement_notifications(rec.id);
    end if;
  end loop;

  return null;
end;
$$;
select verify_function('app_private.tg_announcement__after_write', 'announcement');

create or replace function app_private.tg_announcement_audience__after_write()
returns trigger
language plpgsql
security definer
as $$
-- @plpgsql_check_options: oldtable = oldtable, newtable = newtable
declare
  rec record;
begin
  if TG_OP = 'DELETE' then
    for rec in (
      select distinct announcement_id from oldtable
    ) loop
      perform app_private.queue_announcement_notifications(rec.announcement_id);
    end loop;
  else
    for rec in (
      select distinct announcement_id from newtable
    ) loop
      perform app_private.queue_announcement_notifications(rec.announcement_id);
    end loop;
  end if;
  return null;
end;
$$;
select verify_function('app_private.tg_announcement_audience__after_write', 'announcement_audience');

drop trigger if exists _600_notify_announcement_insert on announcement;
drop trigger if exists _600_notify_announcement_update on announcement;
create trigger _600_notify_announcement_insert
  after insert on announcement
  referencing new table as newtable
  for each statement execute function app_private.tg_announcement__after_write();
create trigger _600_notify_announcement_update
  after update on announcement
  referencing new table as newtable old table as oldtable
  for each statement execute function app_private.tg_announcement__after_write();

drop trigger if exists _600_notify_announcement_audience_insert on announcement_audience;
drop trigger if exists _600_notify_announcement_audience_update on announcement_audience;
create trigger _600_notify_announcement_audience_insert
  after insert on announcement_audience
  referencing new table as newtable
  for each statement execute function app_private.tg_announcement_audience__after_write();
create trigger _600_notify_announcement_audience_update
  after update on announcement_audience
  referencing new table as newtable old table as oldtable
  for each statement execute function app_private.tg_announcement_audience__after_write();
