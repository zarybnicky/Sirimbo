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

alter table if exists announcement
  add column if not exists body_version integer not null default 1;

update announcement
set body_version = coalesce(body_version, 1)
where body_version is null;

create or replace function app_private.tg_announcement__body_version()
  returns trigger
  language plpgsql
as $$
begin
  if tg_op = 'INSERT' then
    if new.body_version is null then
      new.body_version := 1;
    end if;
  else
    if new.body is distinct from old.body then
      new.body_version := coalesce(old.body_version, 1) + 1;
    elsif new.body_version is distinct from old.body_version then
      -- allow manual overrides but keep previous value when unchanged
      new.body_version := coalesce(new.body_version, old.body_version);
    else
      new.body_version := old.body_version;
    end if;
  end if;

  return new;
end;
$$;
select verify_function('app_private.tg_announcement__body_version', 'public.announcement');

drop trigger if exists _050_announcement_body_version on announcement;
create trigger _050_announcement_body_version
  before insert or update on announcement
  for each row execute function app_private.tg_announcement__body_version();

create table if not exists notification (
  id bigint generated by default as identity,
  tenant_id bigint not null default public.current_tenant_id(),
  user_id bigint not null,
  notification_type text not null,
  details jsonb not null default '{}'::jsonb,
  dedupe_key text,
  created_at timestamptz not null default now(),
  read_at timestamptz,
  constraint notification_pkey primary key (id),
  constraint notification_tenant_id_fkey foreign key (tenant_id) references public.tenant(id) on delete cascade,
  constraint notification_user_id_fkey foreign key (user_id) references public.users(u_id) on delete cascade
);

grant all on table notification to anonymous;

alter table notification enable row level security;
select app_private.drop_policies('public.notification');

create policy admin_all on notification to administrator using (true) with check (true);
create policy current_tenant on notification as restrictive using ((tenant_id = public.current_tenant_id()));
create policy view_own on notification for select using ((user_id = public.current_user_id()));
create policy update_own on notification for update using ((user_id = public.current_user_id())) with check ((user_id = public.current_user_id()));

create index if not exists notification_user_created_idx on notification (tenant_id, user_id, created_at desc);
create unique index if not exists notification_dedupe_idx on notification (tenant_id, user_id, notification_type, dedupe_key) where dedupe_key is not null;

create or replace function queue_announcement_notifications(in_announcement_id bigint) returns void
language plpgsql
as $$
declare
  v_tenant_id bigint;
  v_is_visible boolean;
  v_since timestamptz;
  v_until timestamptz;
  v_body_version integer;
  v_dedupe_key text;
  v_user_ids bigint[];
begin
  select tenant_id,
         coalesce(is_visible, false),
         scheduled_since,
         scheduled_until,
         coalesce(body_version, 1)
  into v_tenant_id,
    v_is_visible,
    v_since,
    v_until,
    v_body_version
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

  v_dedupe_key := format('announcement:%s:%s', in_announcement_id, v_body_version);

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
  ),
  recipients as (
    select user_id from role_users
    union
    select user_id from cohort_users
  ),
  inserted as (
    insert into notification (
      tenant_id,
      user_id,
      notification_type,
      details,
      dedupe_key
    )
    select
      v_tenant_id,
      r.user_id,
      'announcement_published',
      jsonb_build_object(
        'announcement_id', in_announcement_id,
        'body_version', v_body_version
      ),
      v_dedupe_key
    from recipients r
    on conflict (tenant_id, user_id, notification_type, dedupe_key) do nothing
    returning user_id
  )
  select array_agg(distinct user_id order by user_id)
  into v_user_ids
  from inserted;

  if v_user_ids is null or array_length(v_user_ids, 1) = 0 then
    return;
  end if;

  perform graphile_worker.add_job(
    'notify_announcement',
    json_build_object(
      'announcement_id', in_announcement_id,
      'body_version', v_body_version,
      'user_ids', v_user_ids
    )
  );
end;
$$;
select verify_function('queue_announcement_notifications');
comment on function queue_announcement_notifications(in_announcement_id bigint) is '@omit';
