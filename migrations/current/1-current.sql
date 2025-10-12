--! split: 1-current.sql

create table if not exists app_private.system_admin_user (
  user_id bigint primary key references public.users(id) on delete cascade,
  created_at timestamptz not null default now(),
  created_by bigint not null default public.current_user_id()
);

comment on table app_private.system_admin_user is '@omit all';
comment on column app_private.system_admin_user.user_id is 'Globally privileged user allowed to manage tenants.';
comment on column app_private.system_admin_user.created_at is 'Timestamp when the system administrator role was granted.';
comment on column app_private.system_admin_user.created_by is 'User that granted the system administrator role.';

grant select on table app_private.system_admin_user to administrator;

drop function if exists app_private.is_system_admin(bigint);
create or replace function app_private.is_system_admin(user_id bigint)
returns boolean
language plpgsql
stable
security definer
set search_path = app_private, public, pg_temp
as $$
declare
  v_is_admin boolean;
begin
  if user_id is null then
    return false;
  end if;

  select exists(
    select 1
    from app_private.system_admin_user sau
    where sau.user_id = user_id
  )
  into v_is_admin;

  return coalesce(v_is_admin, false);
end;
$$;

comment on function app_private.is_system_admin(bigint) is 'Returns true when the given user id has global system administrator privileges.';

grant execute on function app_private.is_system_admin(bigint) to anonymous;

select verify_function('app_private.is_system_admin');

drop function if exists public.system_admin_tenants();
create or replace function public.system_admin_tenants()
returns table (
  id bigint,
  name text,
  description text,
  bank_account text,
  origins text[],
  cz_ico text,
  cz_dic text,
  address public.address_domain,
  membership_count bigint,
  trainer_count bigint,
  administrator_count bigint,
  session_count_last_30_days bigint,
  session_count_per_trainer_last_30_days double precision
)
language plpgsql
stable
security definer
set search_path = public, pg_temp
as $$
begin
  if not app_private.is_system_admin(current_user_id()) then
    raise exception 'permission denied for system admin tenant overview'
      using errcode = '42501';
  end if;

  return query
  select
    t.id,
    t.name,
    t.description,
    t.bank_account,
    t.origins,
    t.cz_ico,
    t.cz_dic,
    t.address,
    membership_counts.membership_count,
    staffing.trainer_count,
    administrators.administrator_count,
    load.session_count_last_30_days,
    load.session_count_per_trainer_last_30_days
  from public.tenant t
  cross join lateral (
    select
      count(*) filter (where tm.status = 'active') as membership_count
    from public.tenant_membership tm
    where tm.tenant_id = t.id
  ) as membership_counts
  cross join lateral (
    select count(*) filter (where tt.active) as trainer_count
    from public.tenant_trainer tt
    where tt.tenant_id = t.id
  ) as staffing
  cross join lateral (
    select count(*) filter (where ta.active) as administrator_count
    from public.tenant_administrator ta
    where ta.tenant_id = t.id
  ) as administrators
  cross join lateral (
    select
      count(*) as session_count_last_30_days,
      case
        when coalesce(staffing.trainer_count, 0) > 0 then count(*)::double precision / staffing.trainer_count::double precision
        else 0::double precision
      end as session_count_per_trainer_last_30_days
    from public.event_instance ei
    where ei.tenant_id = t.id
      and coalesce(ei.is_cancelled, false) = false
      and ei.since >= now() - interval '30 days'
  ) as load
  order by t.name;
end;
$$;

comment on function public.system_admin_tenants() is 'Lists tenants with aggregate membership, staffing, and recent session statistics for system administrators.';

grant execute on function public.system_admin_tenants() to anonymous;

select verify_function('public.system_admin_tenants');

drop function if exists public.system_admin_update_tenant(bigint, text, text, text, text[], public.address_domain, text, text);
create or replace function public.system_admin_update_tenant(
  tenant_id bigint,
  name text default null,
  description text default null,
  bank_account text default null,
  origins text[] default null,
  address public.address_domain default null,
  cz_ico text default null,
  cz_dic text default null
)
returns public.tenant
language plpgsql
volatile
security definer
set search_path = public, pg_temp
as $$
declare
  v_tenant public.tenant;
begin
  if not app_private.is_system_admin(current_user_id()) then
    raise exception 'permission denied for system admin tenant update'
      using errcode = '42501';
  end if;

  update public.tenant t
  set
    name = coalesce(name, t.name),
    description = coalesce(description, t.description),
    bank_account = coalesce(bank_account, t.bank_account),
    origins = coalesce(origins, t.origins),
    address = coalesce(address, t.address),
    cz_ico = coalesce(cz_ico, t.cz_ico),
    cz_dic = coalesce(cz_dic, t.cz_dic)
  where t.id = tenant_id
  returning t.* into v_tenant;

  if not found then
    raise exception 'tenant % not found', tenant_id using errcode = 'P0002';
  end if;

  return v_tenant;
end;
$$;

comment on function public.system_admin_update_tenant(bigint, text, text, text, text[], public.address_domain, text, text)
  is 'Allows system administrators to update tenant metadata without switching tenant context.';

grant execute on function public.system_admin_update_tenant(bigint, text, text, text, text[], public.address_domain, text, text) to anonymous;

select verify_function('public.system_admin_update_tenant');

drop function if exists app_private.create_jwt_token(public.users);
create or replace function app_private.create_jwt_token(u public.users) returns public.jwt_token
    language sql stable
as $$
  with details as (
    select
      users.id,
      user_proxy.person_id as person_id,
      tenant_memberships || tenant_trainers || tenant_administrators as my_tenant_ids,
      cohort_memberships as my_cohort_ids,
      couple_ids as my_couple_ids,
      current_tenant_id() = any(tenant_memberships || tenant_trainers || tenant_administrators) as is_member,
      current_tenant_id() = any(tenant_trainers) as is_trainer,
      current_tenant_id() = any(tenant_administrators) as is_admin,
      app_private.is_system_admin(users.id) as is_system_admin
    from users
    left join user_proxy on user_id = users.id
    left join auth_details on user_proxy.person_id = auth_details.person_id
    where users.id = u.id
  )
  select
    extract(epoch from now() + interval '7 days')::integer,
    u.id,
    current_tenant_id(),
    u.u_login,
    u.u_email,
    array_to_json(array_agg(person_id)) as my_person_ids,
    array_to_json(app_private.array_accum(my_tenant_ids)) as my_tenant_ids,
    array_to_json(app_private.array_accum(my_cohort_ids)) as my_cohort_ids,
    array_to_json(app_private.array_accum(my_couple_ids)) as my_couple_ids,
    bool_or(is_member) as is_member,
    bool_or(is_trainer) as is_trainer,
    bool_or(is_admin) as is_admin,
    bool_or(is_system_admin) as is_system_admin
  from details
  group by id;
$$;

comment on function app_private.create_jwt_token(public.users) is 'Generates the JWT payload including global system administrator flag.';

select verify_function('app_private.create_jwt_token');
