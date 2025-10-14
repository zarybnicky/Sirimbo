--! Previous: sha1:360b3d75f95a2a3fd99225dd7cecc28b93c9d119
--! Hash: sha1:171bf69f137b44c4c114f9269b5f79cc4e73fab8

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
create or replace function app_private.is_system_admin(bigint) returns boolean language sql stable
      security definer set search_path = app_private, public, pg_temp
as $$
  select coalesce(exists(
    select 1 from app_private.system_admin_user sau where user_id = $1
  ), false);
$$;

comment on function app_private.is_system_admin(bigint) is 'Returns true when the given user id has global system administrator privileges.';

grant execute on function app_private.is_system_admin(bigint) to anonymous;

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
      count(*) filter (where tm.active) as membership_count
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


drop function if exists public.system_admin_update_tenant;
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
    name = coalesce(system_admin_update_tenant.name, t.name),
    description = coalesce(system_admin_update_tenant.description, t.description),
    bank_account = coalesce(system_admin_update_tenant.bank_account, t.bank_account),
    origins = coalesce(system_admin_update_tenant.origins, t.origins),
    address = coalesce(system_admin_update_tenant.address, t.address),
    cz_ico = coalesce(system_admin_update_tenant.cz_ico, t.cz_ico),
    cz_dic = coalesce(system_admin_update_tenant.cz_dic, t.cz_dic)
  where t.id = tenant_id
  returning t.* into v_tenant;

  if not found then
    raise exception 'tenant % not found', tenant_id using errcode = 'P0002';
  end if;

  return v_tenant;
end;
$$;

comment on function public.system_admin_update_tenant is 'Allows system administrators to update tenant metadata without switching tenant context.';
grant execute on function public.system_admin_update_tenant to anonymous;

select verify_function('public.system_admin_update_tenant');


DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1
    FROM pg_attribute a
    JOIN pg_type t ON t.typrelid = a.attrelid
    WHERE t.oid = to_regtype('public.jwt_token')
      AND a.attname = 'is_system_admin'
      AND a.attnum > 0
      AND NOT a.attisdropped
  ) THEN
    ALTER TYPE public.jwt_token ADD ATTRIBUTE is_system_admin boolean;
  END IF;
END
$$ LANGUAGE plpgsql;

drop function if exists app_private.create_jwt_token;
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

CREATE or replace FUNCTION public.former_filtered_people(
  is_trainer boolean,
  is_admin boolean,
  in_cohorts bigint[] default null
) RETURNS SETOF person LANGUAGE sql STABLE AS $$
  with details as (
    select
      person.id as person_id,
      array_remove(array_agg(distinct case when cm.tenant_id = current_tenant_id() then cm.cohort_id end), null) as cohort_memberships,
      array_remove(array_agg(distinct case when tt.tenant_id = current_tenant_id() then tt.tenant_id end), null) as tenant_trainers,
      array_remove(array_agg(distinct case when ta.tenant_id = current_tenant_id() then ta.tenant_id end), null) as tenant_administrators
    from person
    join tenant_membership tm on tm.person_id = person.id and tm.tenant_id = current_tenant_id()
    left join cohort_membership cm on cm.person_id = person.id and cm.tenant_id = current_tenant_id() and cm.status = 'expired'
    left join tenant_trainer tt on tt.person_id = person.id and tt.tenant_id = current_tenant_id() and tt.status = 'expired'
    left join tenant_administrator ta on ta.person_id = person.id and ta.tenant_id = current_tenant_id() and ta.status = 'expired'
    where
      tm.status = 'expired'
      and not exists (
        select 1 from tenant_membership active_tm
        where active_tm.person_id = person.id
          and active_tm.tenant_id = current_tenant_id()
          and active_tm.status = 'active'
      )
    group by person.id
  )
  select p.*
  from person p
  join details d on d.person_id = p.id
  where
    case
      when in_cohorts is null then true
      else coalesce(in_cohorts = d.cohort_memberships, false)
        or coalesce(in_cohorts && d.cohort_memberships, false)
    end
    and case
      when is_trainer is null then true
      else is_trainer = (
        current_tenant_id() = any (coalesce(d.tenant_trainers, array[]::bigint[]))
      )
    end
    and case
      when is_admin is null then true
      else is_admin = (
        current_tenant_id() = any (coalesce(d.tenant_administrators, array[]::bigint[]))
      )
    end
  order by p.last_name, p.first_name
$$;
COMMENT ON FUNCTION public.former_filtered_people IS '@omit';
GRANT ALL ON FUNCTION public.former_filtered_people TO anonymous;


grant all on function post_without_cache to administrator;
