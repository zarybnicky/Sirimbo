--! Previous: sha1:f60aa4448284bfe8b511f88163f8967e17db193f
--! Hash: sha1:a20ba927828e25186cc212247b7301fcd725ffbe

--! split: 1-current.sql
create or replace function event_registration_last_attended(reg event_registration) returns timestamptz
    language sql stable as $$
  select max(event_instance.since)
  from event_attendance
  join event_instance on event_instance.id = event_attendance.instance_id
  where event_attendance.registration_id = reg.id
    and event_attendance.status = 'attended'
$$;

grant all on function event_registration_last_attended(event_registration) to anonymous;

drop function if exists my_announcements;
drop function if exists sticky_announcements;
drop function if exists archived_announcements;
drop function if exists my_announcement_new;
drop function if exists sticky_announcement_new;
drop function if exists archived_announcement_new;
drop function if exists my_upozorneni;
drop function if exists sticky_upozorneni;
drop function if exists archived_upozorneni;

drop view if exists announcement;
create or replace view announcement with (security_invoker = true) as
  select
    up_id as id,
    tenant_id,
    up_kdo as author_id,
    up_nadpis as title,
    up_text as body,
    created_at,
    updated_at,
    scheduled_since,
    scheduled_until,
    is_visible,
    false as is_locked,
    sticky as is_sticky
  from upozorneni;

grant all on announcement to anonymous;

comment on view announcement is E'@primaryKey id
@foreignKey (tenant_id) references tenant (id)
@foreignKey (author_id) references users (id)
@simpleCollections both';

comment on column announcement.id is '@hasDefault';
comment on column announcement.title is '@notNull';
comment on column announcement.body is '@notNull';
comment on column announcement.is_locked is E'@notNull
@hasDefault';
comment on column announcement.is_visible is E'@notNull
@hasDefault';
comment on column announcement.tenant_id is '@hasDefault';
comment on column announcement.is_sticky is E'@notNull
@hasDefault';
comment on column announcement.created_at is E'@notNull
@hasDefault';

comment on table upozorneni_skupiny is E'@omit create,update,delete
@foreignKey (ups_id_rodic) references announcement (id)';
comment on constraint upozorneni_skupiny_ups_id_rodic_fkey on upozorneni_skupiny is '@fieldName upozorneni';

CREATE or replace FUNCTION archived_announcements() RETURNS SETOF announcement
    LANGUAGE sql STABLE
    AS $$
  select announcement.* from announcement
  where is_visible = false
    or (scheduled_until is null or scheduled_until >= now())
  order by created_at desc;
$$;
GRANT ALL ON FUNCTION archived_announcements TO anonymous;

CREATE or replace FUNCTION sticky_announcements(
  order_by_updated boolean default false
) RETURNS SETOF announcement
    LANGUAGE sql STABLE
    AS $$
  select announcement.* from announcement
  where is_visible = true and is_sticky = true
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by
    case when order_by_updated then updated_at else created_at end desc,
    created_at desc;
$$;
GRANT ALL ON FUNCTION sticky_announcements TO anonymous;

CREATE or replace FUNCTION my_announcements(
  archive boolean DEFAULT false,
  order_by_updated boolean default false
) RETURNS SETOF announcement LANGUAGE sql STABLE AS $$
  select announcement.* from announcement
  where is_visible = not archive and is_sticky = false
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by
    case when order_by_updated then updated_at else created_at end desc,
    created_at desc;
$$;
GRANT ALL ON FUNCTION my_announcements TO anonymous;



CREATE or replace FUNCTION sticky_announcement_new() RETURNS SETOF announcement
    LANGUAGE sql STABLE
    AS $$
  select announcement.* from announcement
  where is_visible = true and is_sticky = true
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by created_at desc;
$$;
GRANT ALL ON FUNCTION sticky_announcement_new() TO anonymous;

CREATE or replace FUNCTION archived_announcement_new() RETURNS SETOF announcement
    LANGUAGE sql STABLE
    AS $$
  select announcement.* from announcement
  where is_visible = false
    or (scheduled_until is null or scheduled_until >= now())
  order by created_at desc;
$$;
GRANT ALL ON FUNCTION archived_announcement_new() TO anonymous;

CREATE or replace FUNCTION my_announcement_new(archive boolean DEFAULT false) RETURNS SETOF announcement
    LANGUAGE sql STABLE
    AS $$
  select announcement.* from announcement
  where is_visible = not archive and is_sticky = false
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by created_at desc;
$$;
GRANT ALL ON FUNCTION my_announcement_new(archive boolean) TO anonymous;



alter table users
  add column if not exists last_active_at timestamp with time zone,
  add column if not exists last_version text;

drop function if exists get_current_user;


create or replace function get_current_user(version_id text default null) returns users
language sql volatile security definer as $$
  with updated_user as (
    update users
    set
      last_active_at = now(),
      last_version = version_id
    where u_id = nullif(current_setting('jwt.claims.user_id', true), '')::integer
    returning *
  )
  select * from updated_user
  union all
  select *
  from users
  where u_id = nullif(current_setting('jwt.claims.user_id', true), '')::integer
    and not exists (select 1 from updated_user);
$$;

grant all on function get_current_user to anonymous;
