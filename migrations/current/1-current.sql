create or replace function public.event_registration_last_attended(reg public.event_registration)
returns timestamp with time zone
language sql
stable
as $$
  select max(event_instance.since)
  from public.event_attendance
  join public.event_instance on event_instance.id = event_attendance.instance_id
  where event_attendance.registration_id = reg.id
    and event_attendance.status = 'attended'
$$;

grant all on function public.event_registration_last_attended(public.event_registration) to anonymous;

do $$
begin
  if not exists (
    select 1
    from pg_type t
    join pg_namespace n on n.oid = t.typnamespace
    where n.nspname = 'public'
      and t.typname = 'push_notification_platform'
  ) then
    create type public.push_notification_platform as enum ('android', 'ios', 'web');
  end if;
end;
$$;

create table if not exists public.push_notification_channel (
  id bigint generated always as identity primary key,
  tenant_id bigint default public.current_tenant_id() not null,
  user_id bigint not null,
  platform public.push_notification_platform not null,
  provider text not null,
  channel_identifier text not null,
  credentials jsonb default '{}'::jsonb not null,
  is_active boolean default true not null,
  last_registered_at timestamp with time zone default now() not null,
  last_notified_at timestamp with time zone,
  created_at timestamp with time zone default now() not null,
  updated_at timestamp with time zone default now() not null,
  constraint push_notification_channel_provider_check check (provider in ('fcm', 'web_push'))
);

comment on table public.push_notification_channel is 'Device/channel registrations for user-scoped push notifications.';
comment on column public.push_notification_channel.tenant_id is 'Tenant owning the registration; enforced via RLS.';
comment on column public.push_notification_channel.user_id is 'User that should receive pushes on this channel.';
comment on column public.push_notification_channel.platform is 'Target platform for the channel (android, ios, web).';
comment on column public.push_notification_channel.provider is 'Underlying delivery provider (Firebase Cloud Messaging or Web Push).';
comment on column public.push_notification_channel.channel_identifier is 'Opaque identifier (FCM token or Web Push endpoint).';
comment on column public.push_notification_channel.credentials is 'Provider-specific credential payload (e.g. Web Push keys).';

grant all on table public.push_notification_channel to anonymous;

alter table public.push_notification_channel enable row level security;

alter table only public.push_notification_channel
  add constraint push_notification_channel_tenant_id_fkey foreign key (tenant_id) references public.tenant(id) on delete cascade;
alter table only public.push_notification_channel
  add constraint push_notification_channel_user_id_fkey foreign key (user_id) references public.users(u_id) on delete cascade;

create policy push_notification_channel_admin_all on public.push_notification_channel
  to administrator using (true) with check (true);

create policy push_notification_channel_current_tenant on public.push_notification_channel
  as restrictive using ((tenant_id = public.current_tenant_id()));

create policy push_notification_channel_owner_manage on public.push_notification_channel
  for all to member using ((user_id = public.current_user_id()))
  with check ((user_id = public.current_user_id()));

create trigger _100_timestamps
  before insert or update on public.push_notification_channel
  for each row execute function app_private.tg__timestamps();

create index if not exists push_notification_channel_user_id_idx
  on public.push_notification_channel (user_id);

create index if not exists push_notification_channel_tenant_id_idx
  on public.push_notification_channel (tenant_id);

create unique index if not exists push_notification_channel_provider_identifier_key
  on public.push_notification_channel (provider, channel_identifier);
