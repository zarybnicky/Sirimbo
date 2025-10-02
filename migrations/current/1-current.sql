--!include functions/announcement_recipient_user_ids.sql

create or replace function postgraphile_watch.notify_watchers_ddl() returns event_trigger as $$
declare
  ddl_commands json;
begin
  select json_agg(
    json_build_object('schema', schema_name, 'command', command_tag)
  ) into ddl_commands
  from pg_event_trigger_ddl_commands();

  if json_array_length(ddl_commands) > 0 then
    perform pg_notify(
      'postgraphile_watch',
      json_build_object('type', 'ddl', 'payload', ddl_commands)::text
    );
  end if;
end;
$$ language plpgsql;

create or replace function postgraphile_watch.notify_watchers_drop() returns event_trigger as $$
declare
  objects json;
begin
  select json_agg(distinct schema_name) into objects
  from pg_event_trigger_dropped_objects()
  where schema_name <> 'pg_temp';

  if json_array_length(objects) > 0 then
    perform pg_notify(
      'postgraphile_watch',
      json_build_object('type', 'drop', 'payload', objects)::text
    );
  end if;
end;
$$ language plpgsql;

select verify_function('public.active_tenant_member_user_ids');
select verify_function('public.active_tenant_trainer_user_ids');
select verify_function('public.active_tenant_administrator_user_ids');

do $$
begin
  if not exists (
    select 1
    from pg_type t
    join pg_namespace n on n.oid = t.typnamespace
    where t.typname = 'announcement_type_input'
      and n.nspname = 'public'
  ) then
    create type public.announcement_type_input as (
      id bigint,
      up_nadpis text,
      up_text text,
      up_lock boolean,
      is_visible boolean,
      scheduled_since timestamptz,
      scheduled_until timestamptz,
      sticky boolean
    );
  end if;
end;
$$;

do $$
begin
  if not exists (
    select 1
    from pg_type t
    join pg_namespace n on n.oid = t.typnamespace
    where t.typname = 'announcement_cohort_type_input'
      and n.nspname = 'public'
  ) then
    create type public.announcement_cohort_type_input as (
      id bigint,
      cohort_id bigint
    );
  end if;
end;
$$;

do $$
begin
  if not exists (
    select 1
    from pg_type t
    join pg_namespace n on n.oid = t.typnamespace
    where t.typname = 'announcement_audience_type_input'
      and n.nspname = 'public'
  ) then
    create type public.announcement_audience_type_input as (
      id bigint,
      audience_role public.announcement_audience_role
    );
  end if;
end;
$$;

drop function if exists public.upsert_announcement(
  info public.announcement_type_input,
  cohorts public.announcement_cohort_type_input[],
  audiences public.announcement_audience_type_input[]
);

drop function if exists public.queue_announcement_notifications(bigint);
drop function if exists public.tg_upozorneni__after_write();
drop function if exists public.tg_announcement_audience__after_write();
drop function if exists public.tg_upozorneni_skupiny__after_write();

create or replace function public.upsert_announcement(
  info public.announcement_type_input,
  cohorts public.announcement_cohort_type_input[] default null,
  audiences public.announcement_audience_type_input[] default null
) returns public.upozorneni
language plpgsql
as $$
declare
  v_announcement public.upozorneni;
begin
  if info.id is not null then
    update public.upozorneni set
      up_nadpis = info.up_nadpis,
      up_text = info.up_text,
      up_lock = coalesce(info.up_lock, false),
      is_visible = coalesce(info.is_visible, true),
      scheduled_since = info.scheduled_since,
      scheduled_until = info.scheduled_until,
      sticky = coalesce(info.sticky, false)
    where up_id = info.id
    returning * into v_announcement;

    if not found then
      raise exception 'Announcement with id % not found', info.id;
    end if;
  else
    insert into public.upozorneni (
      up_nadpis,
      up_text,
      up_lock,
      is_visible,
      scheduled_since,
      scheduled_until,
      sticky
    )
    values (
      info.up_nadpis,
      info.up_text,
      coalesce(info.up_lock, false),
      coalesce(info.is_visible, true),
      info.scheduled_since,
      info.scheduled_until,
      coalesce(info.sticky, false)
    )
    returning * into v_announcement;
  end if;

  if cohorts is not null then
    with cohort_input as (
      select distinct (c).cohort_id as cohort_id
      from unnest(cohorts) c
      where (c).cohort_id is not null
    )
    delete from public.announcement_audience aa
    where aa.announcement_id = v_announcement.up_id
      and aa.cohort_id is not null
      and not exists (
        select 1 from cohort_input ci where ci.cohort_id = aa.cohort_id
      );

    with cohort_input as (
      select distinct (c).cohort_id as cohort_id
      from unnest(cohorts) c
      where (c).cohort_id is not null
    )
    insert into public.announcement_audience (announcement_id, cohort_id)
    select v_announcement.up_id, ci.cohort_id
    from cohort_input ci
    on conflict (announcement_id, cohort_id) do nothing;
  end if;

  if audiences is not null then
    with role_input as (
      select distinct (a).audience_role as audience_role
      from unnest(audiences) a
      where (a).audience_role is not null
    )
    delete from public.announcement_audience aa
    where aa.announcement_id = v_announcement.up_id
      and aa.audience_role is not null
      and not exists (
        select 1 from role_input ri where ri.audience_role = aa.audience_role
      );

    with role_input as (
      select distinct (a).audience_role as audience_role
      from unnest(audiences) a
      where (a).audience_role is not null
    )
    insert into public.announcement_audience (announcement_id, audience_role)
    select v_announcement.up_id, ri.audience_role
    from role_input ri
    on conflict (announcement_id, audience_role) do nothing;
  end if;

  return v_announcement;
end;
$$;

create or replace function public.queue_announcement_notifications(in_announcement_id bigint)
returns void
language plpgsql
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
  from public.upozorneni
  where up_id = in_announcement_id;

  if not found then
    return;
  end if;

  if not (v_is_visible
      and (v_since is null or v_since <= now())
      and (v_until is null or v_until > now())) then
    return;
  end if;

  with role_flags as (
    select audience_role
    from public.announcement_audience
    where announcement_id = in_announcement_id
      and audience_role is not null
  ),
  role_users as (
    select user_id from (
      select user_id from public.active_tenant_member_user_ids(v_tenant_id) user_id
      where exists (select 1 from role_flags where audience_role = 'member')
      union
      select user_id from public.active_tenant_trainer_user_ids(v_tenant_id) user_id
      where exists (select 1 from role_flags where audience_role = 'trainer')
      union
      select user_id from public.active_tenant_administrator_user_ids(v_tenant_id) user_id
      where exists (select 1 from role_flags where audience_role = 'administrator')
    ) all_roles
  ),
  cohort_users as (
    select distinct u.u_id as user_id
    from public.announcement_audience aa
    join public.cohort_membership cm
      on cm.cohort_id = aa.cohort_id
     and cm.active
    join public.user_proxy up on up.person_id = cm.person_id and up.active
    join public.users u on u.u_id = up.user_id
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
    payload => jsonb_build_object(
      'announcement_id', in_announcement_id,
      'user_ids', v_user_ids
    ),
    job_key => 'announcement:' || in_announcement_id || ':notify'
  );
end;
$$;

create or replace function public.tg_upozorneni__after_write()
returns trigger
language plpgsql
as $$
declare
  rec record;
  old_row record;
  was_published boolean;
  is_published boolean;
begin
  for rec in
    select * from new_table
  loop
    is_published := coalesce(rec.is_visible, false)
      and (rec.scheduled_since is null or rec.scheduled_since <= now())
      and (rec.scheduled_until is null or rec.scheduled_until > now());

    if TG_OP = 'INSERT' then
      was_published := false;
    else
      select * into old_row
      from old_table
      where up_id = rec.up_id;

      if not found then
        was_published := false;
      else
        was_published := coalesce(old_row.is_visible, false)
          and (old_row.scheduled_since is null or old_row.scheduled_since <= now())
          and (old_row.scheduled_until is null or old_row.scheduled_until > now());
      end if;
    end if;

    if is_published and not was_published then
      perform public.queue_announcement_notifications(rec.up_id);
    end if;
  end loop;

  return null;
end;
$$;

create or replace function public.tg_announcement_audience__after_write()
returns trigger
language plpgsql
as $$
declare
  rec record;
begin
  if TG_OP = 'DELETE' then
    for rec in (
      select distinct announcement_id from old_table
    ) loop
      perform public.queue_announcement_notifications(rec.announcement_id);
    end loop;
  else
    for rec in (
      select distinct announcement_id from new_table
    ) loop
      perform public.queue_announcement_notifications(rec.announcement_id);
    end loop;
  end if;
  return null;
end;
$$;

select verify_function('public.upsert_announcement');
select verify_function('public.queue_announcement_notifications');
select verify_function('public.tg_upozorneni__after_write');
select verify_function('public.tg_announcement_audience__after_write');

drop trigger if exists _600_queue_announcement_notifications on public.upozorneni;
drop trigger if exists _600_update_queue_announcement_notifications on public.upozorneni;
drop trigger if exists _600_queue_announcement_audience on public.announcement_audience;
drop trigger if exists _600_update_queue_announcement_audience on public.announcement_audience;
drop trigger if exists _600_queue_announcement_groups on public.upozorneni_skupiny;
drop trigger if exists _600_notify_upozorneni_skupiny on public.upozorneni_skupiny;

drop trigger if exists _600_notify_upozorneni on public.upozorneni;
create trigger _600_notify_upozorneni
  after insert or update on public.upozorneni
  referencing new table as new_table old table as old_table
  for each statement execute function public.tg_upozorneni__after_write();

drop trigger if exists _600_notify_announcement_audience on public.announcement_audience;
create trigger _600_notify_announcement_audience
  after insert or update or delete on public.announcement_audience
  referencing new table as new_table old table as old_table
  for each statement execute function public.tg_announcement_audience__after_write();


grant all on function public.upsert_announcement to anonymous;
