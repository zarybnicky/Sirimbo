create or replace function app_private.announcement_status_next(
  ts timestamptz,
  scheduled_since timestamptz,
  scheduled_until timestamptz,
  current_status public.announcement_status
) returns public.announcement_status
  language sql immutable
as $$
  select case
    when current_status in ('draft', 'archived') then current_status
    when scheduled_until is not null and ts >= scheduled_until then 'archived'
    when scheduled_since is not null and ts < scheduled_since then 'scheduled'
    else 'published'
  end::public.announcement_status;
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

drop trigger if exists _300_status on public.announcement;
create trigger _300_status
  before insert or update of status, scheduled_since, scheduled_until on public.announcement
  for each row execute function app_private.tg_announcement__status();
