
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

  with audience as (
    select array_agg(audience_role) filter (where audience_role is not null) as roles,
      array_agg(cohort_id) filter (where cohort_id is not null) as cohort_ids
    from announcement_audience
    where announcement_id = in_announcement_id
  )
  select array_agg(distinct up.user_id order by up.user_id)
  into v_user_ids
  from app_private.tenant_relation r
  join user_proxy up on up.person_id = r.person_id and up.status = 'active'
  cross join audience a
  where r.tenant_id = (select current_tenant_id())
    and (r.current_roles && a.roles or r.current_cohort_ids && a.cohort_ids);

  if v_user_ids is null then
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
