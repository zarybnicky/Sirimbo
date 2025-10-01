--!include functions/register_without_invitation.sql

create or replace view announcement as
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
    up_lock as is_locked,
    is_visible,
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
comment on column announcement.tenant_id is '@hasDefault';
comment on column announcement.is_sticky is E'@notNull
@hasDefault';
comment on column announcement.created_at is E'@notNull
@hasDefault';

comment on table public.upozorneni_skupiny is E'@omit create,update,delete
@foreignKey (ups_id_rodic) references announcement (id)';
comment on constraint upozorneni_skupiny_ups_id_rodic_fkey on upozorneni_skupiny is '@fieldName upozorneni';

drop function if exists sticky_announcement_new;
CREATE or replace FUNCTION public.sticky_announcement_new() RETURNS SETOF public.announcement
    LANGUAGE sql STABLE
    AS $$
  select announcement.* from announcement
  where is_visible = true and is_sticky = true
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by created_at desc;
$$;
GRANT ALL ON FUNCTION public.sticky_announcement_new() TO anonymous;

drop function if exists archived_announcement_new;
CREATE or replace FUNCTION public.archived_announcement_new() RETURNS SETOF public.announcement
    LANGUAGE sql STABLE
    AS $$
  select announcement.* from announcement
  where is_visible = false
    or (scheduled_until is null or scheduled_until >= now())
  order by created_at desc;
$$;
GRANT ALL ON FUNCTION public.archived_announcement_new() TO anonymous;

drop function if exists my_announcement_new;
CREATE or replace FUNCTION public.my_announcement_new(archive boolean DEFAULT false) RETURNS SETOF public.announcement
    LANGUAGE sql STABLE
    AS $$
  select announcement.* from announcement
  where is_visible = not archive and is_sticky = false
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by created_at desc;
$$;
GRANT ALL ON FUNCTION public.my_announcement_new(archive boolean) TO anonymous;


CREATE or replace FUNCTION public.sticky_upozorneni() RETURNS SETOF public.upozorneni
    LANGUAGE sql STABLE
    AS $$
  select upozorneni.* from upozorneni
  where is_visible = true and sticky = true
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by created_at desc;
$$;
GRANT ALL ON FUNCTION public.sticky_upozorneni() TO anonymous;

CREATE or replace FUNCTION public.archived_upozorneni() RETURNS SETOF public.upozorneni
    LANGUAGE sql STABLE
    AS $$
  select upozorneni.* from upozorneni
  where is_visible = false
    or (scheduled_until is null or scheduled_until >= now())
  order by created_at desc;
$$;
GRANT ALL ON FUNCTION public.archived_upozorneni() TO anonymous;

CREATE or replace FUNCTION public.my_upozorneni(archive boolean DEFAULT false) RETURNS SETOF public.upozorneni
    LANGUAGE sql STABLE
    AS $$
  select upozorneni.* from upozorneni
  where is_visible = not archive and sticky = false
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by created_at desc;
$$;
GRANT ALL ON FUNCTION public.my_upozorneni(archive boolean) TO anonymous;
