
drop function if exists my_event_instances_for_range;
drop function if exists event_instances_for_range;

alter table if exists public.event_instance
  drop column if exists range,
  add column range tstzrange generated always as (tstzrange(since, until, '[)'::text)) stored not null;

--!include functions/event_instances_for_range.sql
--!include functions/my_event_instances_for_range.sql



create schema if not exists wdsf;
grant all on schema wdsf to postgres;
grant all on schema wdsf to olymp;

create schema if not exists csts;
grant all on schema csts to postgres;
grant all on schema csts to olymp;

create table if not exists public.calendar_feed_subscription (
  id bigserial primary key,
  tenant_id bigint default public.current_tenant_id() not null,
  user_id bigint default public.current_user_id() not null,
  token uuid default gen_random_uuid() not null,
  only_type public.event_type,
  only_mine boolean default false not null,
  start_offset_days integer default 0 not null,
  end_offset_days integer,
  created_at timestamp with time zone default now() not null,
  updated_at timestamp with time zone default now() not null,
  constraint calendar_feed_subscription_token_key unique (token)
);

comment on table public.calendar_feed_subscription is '@simpleCollections only';

grant all on table public.calendar_feed_subscription to anonymous;

alter table public.calendar_feed_subscription enable row level security;

do $$
begin
  if not exists (
    select 1 from pg_policies where schemaname = 'public' and tablename = 'calendar_feed_subscription' and policyname = 'calendar_feed_subscription_admin_all'
  ) then
    create policy calendar_feed_subscription_admin_all on public.calendar_feed_subscription
      to administrator using (true) with check (true);
  end if;
end;
$$;

do $$
begin
  if not exists (
    select 1 from pg_policies where schemaname = 'public' and tablename = 'calendar_feed_subscription' and policyname = 'calendar_feed_subscription_manage_own'
  ) then
    create policy calendar_feed_subscription_manage_own on public.calendar_feed_subscription
      using (user_id = public.current_user_id())
      with check (user_id = public.current_user_id());
  end if;
end;
$$;

do $$
begin
  if not exists (
    select 1 from pg_trigger where tgname = '_100_timestamps' and tgrelid = 'public.calendar_feed_subscription'::regclass
  ) then
    create trigger _100_timestamps before insert or update on public.calendar_feed_subscription
      for each row execute function app_private.tg__timestamps();
  end if;
end;
$$;

create index if not exists calendar_feed_subscription_user_id_idx on public.calendar_feed_subscription using btree (user_id);
create unique index if not exists calendar_feed_subscription_token_idx on public.calendar_feed_subscription using btree (token);
