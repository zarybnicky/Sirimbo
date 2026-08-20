-- Announcement publication is a lifecycle; audience and ownership remain RLS concerns.
do $$
begin
  if not exists (
    select
    from pg_catalog.pg_type type
    join pg_catalog.pg_namespace namespace on namespace.oid = type.typnamespace
    where namespace.nspname = 'public' and type.typname = 'announcement_status'
  ) then
    create type public.announcement_status as enum (
      'draft', 'scheduled', 'published', 'archived'
    );
  end if;
end
$$;

alter table public.announcement
  add column if not exists status public.announcement_status not null default 'draft';

do $$
begin
  if exists (
    select from information_schema.columns
    where table_schema = 'public'
      and table_name = 'announcement'
      and column_name = 'is_visible'
  ) then
    update public.announcement
    set status = case
      when not is_visible then 'archived'
      when scheduled_until is not null and now() >= scheduled_until then 'archived'
      when scheduled_since is not null and now() < scheduled_since then 'scheduled'
      else 'published'
    end::public.announcement_status;
  end if;
end
$$;

alter table public.announcement
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
    alter type public.announcement_type_input
      add attribute status public.announcement_status;
  end if;
end
$$;

--!include functions/announcement_lifecycle.sql
--!include functions/upsert_announcement.sql
--!include functions/cron_update_memberships.sql
--!include functions/queue_announcement_notification.sql
--!include policies/announcement.sql

create index if not exists announcement_status_created_at_idx
  on public.announcement (tenant_id, status, is_sticky, created_at desc);

-- TODO(next migration): remove my_announcements, is_visible, is_locked, and their input attributes.
