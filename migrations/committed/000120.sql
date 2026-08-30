--! Previous: sha1:ec2e1175abf5b4b80a2875abd9c4c27f9b474fd7
--! Hash: sha1:b36764f99671aab04a196359a3331b7c7261922d

--! split: 1-current.sql
do $$
begin
  if not exists (
    select
    from pg_catalog.pg_type type
    join pg_catalog.pg_namespace namespace on namespace.oid = type.typnamespace
    where namespace.nspname = 'public' and type.typname = 'announcement_status'
  ) then
    create type announcement_status as enum (
      'draft', 'scheduled', 'published', 'archived'
    );
  end if;
end
$$;

alter table announcement
  add column if not exists status announcement_status not null default 'draft';

do $$
begin
  if exists (
    select from information_schema.columns
    where table_schema = 'public'
      and table_name = 'announcement'
      and column_name = 'is_visible'
  ) then
    update announcement
    set status = case
      when not is_visible then 'archived'
      when scheduled_until is not null and now() >= scheduled_until then 'archived'
      when scheduled_since is not null and now() < scheduled_since then 'scheduled'
      else 'published'
    end::announcement_status;
  end if;
end
$$;

alter table announcement
  drop constraint if exists announcement_schedule_check,
  add constraint announcement_schedule_check check (
    scheduled_since is null
    or scheduled_until is null
    or scheduled_since < scheduled_until
  );

do $$
begin
  if not exists (
    select
    from pg_catalog.pg_attribute attribute
    where attribute.attrelid = 'public.announcement_type_input'::regclass
      and attribute.attname = 'status'
      and not attribute.attisdropped
  ) then
    alter type announcement_type_input
      add attribute status announcement_status;
  end if;
end
$$;

--! Included functions/announcement_lifecycle.sql
create or replace function app_private.announcement_status_next(
  ts timestamptz,
  scheduled_since timestamptz,
  scheduled_until timestamptz,
  current_status announcement_status
) returns announcement_status
  language sql immutable
as $$
  select case
    when current_status in ('draft', 'archived') then current_status
    when scheduled_until is not null and ts >= scheduled_until then 'archived'
    when scheduled_since is not null and ts < scheduled_since then 'scheduled'
    else 'published'
  end::announcement_status;
$$;

create or replace function app_private.tg_announcement__status()
returns trigger
language plpgsql
set search_path to pg_catalog, public, app_private
as $$
begin
  new.status = app_private.announcement_status_next(
    now(), new.scheduled_since, new.scheduled_until, new.status
  );
  return new;
end;
$$;

drop trigger if exists _300_status on announcement;
create trigger _300_status
  before insert or update of status, scheduled_since, scheduled_until on announcement
  for each row execute function app_private.tg_announcement__status();
--! EndIncluded functions/announcement_lifecycle.sql
--! Included functions/cron_update_memberships.sql
create or replace function app_private.relationship_status_next(ts timestamptz, range tstzrange, current relationship_status)
  returns relationship_status
  language sql
  immutable
as $$
  select case
    when ts < lower(range) then 'pending'
    when not upper_inf(range) and ts >= upper(range) then 'expired'
    when range @> ts then 'active'
    else current
  end
$$;

create or replace function app_private.cron_update_memberships() returns void language sql
as $$
  UPDATE user_proxy SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE couple SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE cohort_membership SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE tenant_membership SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE tenant_trainer SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE tenant_administrator SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE announcement
  SET status = app_private.announcement_status_next(now(), scheduled_since, scheduled_until, status)
  WHERE status IN ('scheduled', 'published')
    AND status IS DISTINCT FROM app_private.announcement_status_next(now(), scheduled_since, scheduled_until, status);
$$;
--! EndIncluded functions/cron_update_memberships.sql
--! Included functions/queue_announcement_notification.sql
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
--! EndIncluded functions/queue_announcement_notification.sql
--! Included functions/visible_announcement_ids.sql
create or replace function app_private.visible_announcement_ids()
  returns setof bigint
  language sql stable
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  with eligible as (
    select announcement.id
    from announcement
    where pg_catalog.pg_has_role(pg_catalog.current_setting('role'), 'member', 'member')
      and announcement.tenant_id = (select current_tenant_id())
      and announcement.status in ('published', 'archived')
  )
  select eligible.id
  from eligible
  where not exists (
    select from announcement_audience where announcement_id = eligible.id
  )
  union all
  select announcement_id
  from announcement_audience join eligible on eligible.id = announcement_id
  where (
    cohort_id in (
      select cohort_id from current_cohort_membership where person_id = any ((select current_person_ids())::bigint[])
    )
    or audience_role = 'member' and exists (
      select from current_tenant_membership where person_id = any ((select current_person_ids())::bigint[])
    )
    or audience_role = 'trainer' and exists (
      select from current_tenant_trainer where person_id = any ((select current_person_ids())::bigint[])
    )
    or audience_role = 'administrator' and exists (
      select from current_tenant_administrator where person_id = any ((select current_person_ids())::bigint[])
    )
  );
$$;

grant execute on function app_private.visible_announcement_ids() to anonymous;
--! EndIncluded functions/visible_announcement_ids.sql
--! Included policies/announcement.sql
select app_private.drop_policies('public.announcement');

create policy current_tenant on announcement as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on announcement to administrator using (true);
create policy trainer_manage_own on announcement to trainer
  using (author_id = (select current_user_id()));
create policy member_view on announcement for select to member
  using (id in (select app_private.visible_announcement_ids()));

select app_private.drop_policies('public.announcement_audience');

create policy current_tenant on announcement_audience as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on announcement_audience to administrator using (true);
create policy trainer_manage on announcement_audience to trainer using (true);
create policy member_view on announcement_audience for select to member using (true);

grant all on table announcement, announcement_audience to anonymous;
--! EndIncluded policies/announcement.sql

create index if not exists announcement_status_created_at_idx
  on announcement (tenant_id, status, is_sticky, created_at desc);

drop function if exists my_announcements;

alter table announcement
  drop column if exists is_locked,
  drop column if exists is_visible;

--! split: 2-files.sql
drop trigger if exists _900_delete_object on file;
drop function if exists app_private.tg_file__delete;
drop trigger if exists _500_sync_attachments_insert on announcement;
drop trigger if exists _500_sync_attachments_update on announcement;
drop trigger if exists _500_sync_attachments_insert on aktuality;
drop trigger if exists _500_sync_attachments_update on aktuality;
drop function if exists app_private.tg_announcement__sync_attachments;
drop function if exists app_private.tg_aktuality__sync_attachments;
drop table if exists article_attachment;
drop table if exists announcement_attachment;
drop table if exists file;


create table if not exists file (
  id bigint generated always as identity primary key,
  tenant_id bigint not null default current_tenant_id() references tenant(id) on delete cascade,
  object_key text not null unique,
  name text not null,
  content_type text,
  byte_size bigint,
  uploaded_by bigint default current_user_id() references users(id) on delete set null,
  uploaded_at timestamptz,
  created_at timestamptz not null default now()
);

create unique index if not exists file_tenant_id_id_idx on file (tenant_id, id);

grant all on table file to anonymous;
alter table file enable row level security;
comment on table file is '@omit create,update,delete';


create unique index if not exists announcement_tenant_id_id_idx on announcement (tenant_id, id);

create table if not exists announcement_attachment (
  tenant_id bigint not null default current_tenant_id(),
  announcement_id bigint not null,
  file_id bigint not null,
  inline boolean not null default false,
  primary key (tenant_id, announcement_id, file_id),
  constraint announcement_attachment_announcement_fk
    foreign key (tenant_id, announcement_id) references announcement(tenant_id, id) on delete cascade,
  constraint announcement_attachment_file_fk
    foreign key (tenant_id, file_id) references file(tenant_id, id) on delete cascade
);

create index if not exists announcement_attachment_file_idx on announcement_attachment (tenant_id, file_id);

comment on constraint announcement_attachment_announcement_fk on announcement_attachment
  is E'@fieldName announcement\n@foreignFieldName attachments';
comment on constraint announcement_attachment_file_fk on announcement_attachment
  is E'@fieldName file\n@foreignFieldName announcementAttachments';

grant all on table announcement_attachment to anonymous;
alter table announcement_attachment enable row level security;
comment on table announcement_attachment is '@omit create,update,delete';


create unique index if not exists aktuality_tenant_id_id_idx on aktuality (tenant_id, id);

create table if not exists article_attachment (
  tenant_id bigint not null default current_tenant_id(),
  aktuality_id bigint not null,
  file_id bigint not null,
  inline boolean not null default false,
  primary key (tenant_id, aktuality_id, file_id),
  constraint article_attachment_article_fk
    foreign key (tenant_id, aktuality_id) references aktuality(tenant_id, id) on delete cascade,
  constraint article_attachment_file_fk
    foreign key (tenant_id, file_id) references file(tenant_id, id) on delete cascade
);

grant all on table article_attachment to anonymous;
alter table article_attachment enable row level security;
comment on table article_attachment is '@omit create,update,delete';

create index if not exists article_attachment_file_idx on article_attachment (tenant_id, file_id);

comment on constraint article_attachment_article_fk on article_attachment is E'
@fieldName article
@foreignFieldName attachments';
comment on constraint article_attachment_file_fk on article_attachment is E'
@fieldName file
@foreignFieldName articleAttachments';

--! Included functions/visible_file_ids.sql
create or replace function app_private.visible_file_ids()
  returns setof bigint
  language sql stable
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  select f.file_id
  from announcement_attachment f
  join app_private.visible_announcement_ids() a(id) on a.id = f.announcement_id
  where f.tenant_id = (select current_tenant_id())

  union all

  select f.file_id
  from article_attachment f
  join aktuality a on id = f.aktuality_id and a.tenant_id = f.tenant_id
  where a.tenant_id = (select current_tenant_id())
    and a.is_visible;
$$;

grant execute on function app_private.visible_file_ids() to anonymous;
--! EndIncluded functions/visible_file_ids.sql
--! Included policies/file.sql
select app_private.drop_policies('public.file');
create policy current_tenant on file as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on file to administrator
  using (true);
create policy uploader_manage on file to trainer
  using (uploaded_by = (select current_user_id()));
create policy visible on file for select
  using (id in (select id from app_private.visible_file_ids() visible(id)));

select app_private.drop_policies('public.announcement_attachment');
create policy current_tenant on announcement_attachment as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on announcement_attachment to administrator
  using (true);
create policy trainer_manage on announcement_attachment to trainer
  using (announcement_id in (
    select id from announcement where author_id = (select current_user_id())));
create policy member_view on announcement_attachment for select
  using (announcement_id in (select id from announcement));

select app_private.drop_policies('public.article_attachment');
create policy current_tenant on article_attachment as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on article_attachment to administrator
  using (true);
create policy public_view on article_attachment for select
  using (aktuality_id in (select id from aktuality));
--! EndIncluded policies/file.sql
--! Included functions/tg_file__delete.sql
create or replace function app_private.tg_file__delete()
returns trigger
language plpgsql
security definer
set search_path to pg_catalog, public, pg_temp
as $$
begin
  perform graphile_worker.add_job(
    'delete_file',
    json_build_object('object_key', old.object_key)
  );
  return old;
end;
$$;

drop trigger if exists _900_delete_object on file;
create trigger _900_delete_object
  after delete on file
  for each row execute function app_private.tg_file__delete();
--! EndIncluded functions/tg_file__delete.sql
--! Included functions/tg_announcement__sync_attachments.sql
create or replace function app_private.tg_announcement__sync_attachments()
  returns trigger
  language plpgsql
  as $$
declare
  file_ids bigint[];
begin
  select coalesce(array_agg(distinct file.id), '{}'::bigint[])
  into file_ids
  from regexp_matches(coalesce(new.body, ''), '/f/([0-9]+)/', 'g') match(parts)
  join file on file.id::text = match.parts[1]
    and file.tenant_id = new.tenant_id
    and file.uploaded_at is not null;

  delete from announcement_attachment
  where announcement_id = new.id
    and inline
    and file_id <> all(file_ids);

  insert into announcement_attachment (tenant_id, announcement_id, file_id, inline)
  select new.tenant_id, new.id, input.file_id, true
  from unnest(file_ids) input(file_id)
  on conflict (tenant_id, announcement_id, file_id) do nothing;

  return new;
end;
$$;

comment on function app_private.tg_announcement__sync_attachments()
  is 'Synchronizes inline file references from announcement content.';

drop trigger if exists _500_sync_attachments_insert on announcement;
create trigger _500_sync_attachments_insert
  after insert on announcement
  for each row execute function app_private.tg_announcement__sync_attachments();

drop trigger if exists _500_sync_attachments_update on announcement;
create trigger _500_sync_attachments_update
  before update of body on announcement
  for each row execute function app_private.tg_announcement__sync_attachments();
--! EndIncluded functions/tg_announcement__sync_attachments.sql
--! Included functions/tg_aktuality__sync_attachments.sql
create or replace function app_private.tg_aktuality__sync_attachments()
  returns trigger
  language plpgsql
  as $$
declare
  file_ids bigint[];
begin
  select coalesce(array_agg(distinct file.id), '{}'::bigint[])
  into file_ids
  from regexp_matches(
    concat_ws(' ', new.at_preview, new.at_text, new.title_photo_url),
    '/f/([0-9]+)/',
    'g'
  ) match(parts)
  join file on file.id::text = match.parts[1]
    and file.tenant_id = new.tenant_id
    and file.uploaded_at is not null;

  delete from article_attachment
  where aktuality_id = new.id
    and inline
    and file_id <> all(file_ids);

  insert into article_attachment (tenant_id, aktuality_id, file_id, inline)
  select new.tenant_id, new.id, input.file_id, true
  from unnest(file_ids) input(file_id)
  on conflict (tenant_id, aktuality_id, file_id) do nothing;

  return new;
end;
$$;

comment on function app_private.tg_aktuality__sync_attachments()
  is 'Synchronizes inline file references from article content.';

drop trigger if exists _500_sync_attachments_insert on aktuality;
create trigger _500_sync_attachments_insert
  after insert on aktuality
  for each row execute function app_private.tg_aktuality__sync_attachments();

drop trigger if exists _500_sync_attachments_update on aktuality;
create trigger _500_sync_attachments_update
  before update of at_preview, at_text, title_photo_url on aktuality
  for each row execute function app_private.tg_aktuality__sync_attachments();
--! EndIncluded functions/tg_aktuality__sync_attachments.sql
--! Included functions/upsert_announcement.sql
drop function if exists upsert_announcement;
drop type if exists announcement_type_input;
drop type if exists announcement_audience_type_input;

create type announcement_type_input as (
  id bigint,
  title text,
  body text,
  is_sticky boolean,
  scheduled_since timestamptz,
  scheduled_until timestamptz,
  status announcement_status
);

create type announcement_audience_type_input as (
  id bigint,
  cohort_id bigint,
  audience_role announcement_audience_role
);

create or replace function upsert_announcement(
  info announcement_type_input,
  audiences announcement_audience_type_input[] default null,
  attachments bigint[] default null
)
  returns announcement language plpgsql
as $$
declare
  result announcement;
begin
  if info.id is null then
    insert into announcement (
      title, body, status, is_sticky, scheduled_since, scheduled_until
    ) values (
      info.title,
      info.body,
      coalesce(info.status, 'draft'),
      coalesce(info.is_sticky, false),
      info.scheduled_since,
      info.scheduled_until
    ) returning * into result;
  else
    select * into result from announcement where id = info.id;
    if not found then
      raise exception 'Announcement with id % not found', info.id;
    end if;
  end if;

  if attachments is not null then
    select coalesce(array_agg(id), '{}'::bigint[])
    into attachments
    from file
    where id = any(attachments)
      and tenant_id = result.tenant_id
      and uploaded_at is not null;

    delete from announcement_attachment
    where announcement_id = result.id
      and not inline
      and file_id <> all(attachments);

    insert into announcement_attachment (tenant_id, announcement_id, file_id, inline)
    select result.tenant_id, result.id, file_id, false
    from unnest(attachments) input(file_id)
    on conflict (tenant_id, announcement_id, file_id)
    do update set inline = false;
  end if;

  if info.id is not null then
    -- Make sure the update trigger re-populates inline references
    update announcement set
      title = info.title,
      body = info.body,
      status = coalesce(info.status, status),
      is_sticky = coalesce(info.is_sticky, false),
      scheduled_since = info.scheduled_since,
      scheduled_until = info.scheduled_until
    where id = info.id
    returning * into result;
  end if;

  if audiences is not null then
    with input as (
      select distinct (item).id, (item).cohort_id, (item).audience_role
      from unnest(audiences) item
    )
    delete from announcement_audience audience
    using input
    where audience.announcement_id = result.id
      and audience.id = input.id
      and input.id is not null
      and input.cohort_id is null
      and input.audience_role is null;

    with input as (
      select distinct (item).id, (item).cohort_id, (item).audience_role
      from unnest(audiences) item
    )
    update announcement_audience audience
    set cohort_id = input.cohort_id,
        audience_role = input.audience_role
    from input
    where audience.announcement_id = result.id
      and audience.id = input.id
      and input.id is not null
      and ((input.cohort_id is not null) <> (input.audience_role is not null))
      and (audience.cohort_id, audience.audience_role)
        is distinct from (input.cohort_id, input.audience_role);

    with input as (
      select distinct (item).cohort_id
      from unnest(audiences) item
      where (item).id is null
        and (item).cohort_id is not null
        and (item).audience_role is null
    )
    insert into announcement_audience (announcement_id, cohort_id)
    select result.id, input.cohort_id
    from input
    on conflict (announcement_id, cohort_id) do nothing;

    with input as (
      select distinct (item).audience_role
      from unnest(audiences) item
      where (item).id is null
        and (item).cohort_id is null
        and (item).audience_role is not null
    )
    insert into announcement_audience (announcement_id, audience_role)
    select result.id, input.audience_role
    from input
    on conflict (announcement_id, audience_role) do nothing;
  end if;

  return result;
end;
$$;

grant all on function upsert_announcement to anonymous;
--! EndIncluded functions/upsert_announcement.sql
--! Included functions/upsert_article.sql
drop function if exists upsert_article;
drop type if exists article_type_input;

create type article_type_input as (
  id bigint,
  title text,
  preview text,
  body text,
  title_photo_url text,
  is_visible boolean
);

create or replace function upsert_article(info article_type_input, attachments bigint[] default null)
  returns aktuality language plpgsql
  as $$
declare
  result aktuality;
begin
  if info.id is null then
    insert into aktuality (
      at_jmeno, at_preview, at_text, title_photo_url, is_visible
    ) values (
      info.title,
      coalesce(info.preview, ''),
      coalesce(info.body, ''),
      info.title_photo_url,
      coalesce(info.is_visible, true)
    ) returning * into result;
  else
    select * into result from aktuality where id = info.id;

    if not found then
      raise exception 'Article with id % not found', info.id;
    end if;
  end if;

  if attachments is not null then
    select coalesce(array_agg(id), '{}'::bigint[])
    into attachments
    from file
    where id = any(attachments)
      and tenant_id = result.tenant_id
      and uploaded_at is not null;

    delete from article_attachment
    where aktuality_id = result.id
      and not inline
      and file_id <> all(attachments);

    insert into article_attachment (tenant_id, aktuality_id, file_id, inline)
    select result.tenant_id, result.id, file_id, false
    from unnest(attachments) input(file_id)
    on conflict (tenant_id, aktuality_id, file_id)
    do update set inline = false;
  end if;

  if info.id is not null then
    -- Make sure the update trigger re-populates inline references
    update aktuality set
      at_jmeno = info.title,
      at_preview = coalesce(info.preview, ''),
      at_text = coalesce(info.body, ''),
      title_photo_url = info.title_photo_url,
      is_visible = coalesce(info.is_visible, true)
    where id = info.id
    returning * into result;
  end if;

  return result;
end;
$$;

grant all on function upsert_article to anonymous;
--! EndIncluded functions/upsert_article.sql
