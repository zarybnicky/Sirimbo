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

alter table public.users
  add column if not exists last_active_at timestamp with time zone,
  add column if not exists last_version text;

drop function if exists public.get_current_user();

create or replace function public.get_current_user(version_id text default null)
returns public.users
language sql
volatile
security definer
as $$
  with updated_user as (
    update public.users
    set
      last_active_at = now(),
      last_version = version_id
    where u_id = nullif(current_setting('jwt.claims.user_id', true), '')::integer
    returning *
  )
  select * from updated_user
  union all
  select *
  from public.users
  where u_id = nullif(current_setting('jwt.claims.user_id', true), '')::integer
    and not exists (select 1 from updated_user);
$$;

grant all on function public.get_current_user(text) to anonymous;

select verify_function('get_current_user');
