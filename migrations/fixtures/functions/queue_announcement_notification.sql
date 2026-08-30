
create or replace function app_private.queue_announcement_notifications(in_announcement_id bigint)
  returns void
  language plpgsql
  security definer
  set search_path to pg_catalog, public, pg_temp
as $$
declare
  v_user_ids bigint[];
begin
  if not exists (
    select from announcement
    where id = in_announcement_id and status = 'published'
  ) then
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
    select distinct x.person_id
    from role_flags rf
    join lateral (
      select tm.person_id from current_tenant_membership tm where rf.has_member
      union all
      select tt.person_id from current_tenant_trainer tt where rf.has_trainer
      union all
      select ta.person_id from current_tenant_administrator ta where rf.has_administrator
    ) x on true
  ),
  role_users as (
    select distinct u.id as user_id
    from role_people rp
    join user_proxy up on up.person_id = rp.person_id and up.status = 'active'
    join users u on u.id = up.user_id
  ),
  cohort_users as (
    select distinct u.id as user_id
    from announcement_audience aa
    join current_cohort_membership cm on cm.cohort_id = aa.cohort_id
    join user_proxy up on up.person_id = cm.person_id and up.status = 'active'
    join users u on u.id = up.user_id
    where aa.announcement_id = in_announcement_id
      and aa.cohort_id is not null
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

grant all on function app_private.queue_announcement_notifications to anonymous;

create or replace function app_private.tg_announcement__after_write()
  returns trigger
  language plpgsql
  security definer
  set search_path to pg_catalog, public, pg_temp
as $$
-- @plpgsql_check_options: oldtable = oldtable, newtable = newtable
declare
  rec record;
begin
  for rec in
    select * from newtable
  loop
    if rec.status = 'published' then
      if TG_OP = 'INSERT' then
        perform app_private.queue_announcement_notifications(rec.id);
      elsif not exists (select from oldtable where id = rec.id and status = 'published') then
        perform app_private.queue_announcement_notifications(rec.id);
      end if;
    end if;
  end loop;

  return null;
end;
$$;

create or replace function app_private.tg_announcement_audience__after_write()
  returns trigger
  language plpgsql
  security definer
  set search_path to pg_catalog, public, pg_temp
as $$
-- @plpgsql_check_options: oldtable = oldtable, newtable = newtable
declare
  rec record;
begin
  if TG_OP = 'DELETE' then
    for rec in (select distinct announcement_id from oldtable) loop
      perform app_private.queue_announcement_notifications(rec.announcement_id);
    end loop;
  else
    for rec in (select distinct announcement_id from newtable) loop
      perform app_private.queue_announcement_notifications(rec.announcement_id);
    end loop;
  end if;
  return null;
end;
$$;

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

drop trigger if exists _600_notify_announcement_audience_delete on announcement_audience;
create trigger _600_notify_announcement_audience_delete
  after delete on announcement_audience
  referencing old table as oldtable
  for each statement execute function app_private.tg_announcement_audience__after_write();
