drop function if exists public.active_tenant_member_user_ids(bigint);
create or replace function public.active_tenant_member_user_ids(in_tenant_id bigint)
returns setof bigint
language sql
stable
as $$
  select distinct u.u_id
  from public.tenant_membership tm
  join public.user_proxy up on up.person_id = tm.person_id and up.active
  join public.users u on u.u_id = up.user_id
  where tm.tenant_id = in_tenant_id
    and tm.active
    and u.tenant_id = in_tenant_id;
$$;

drop function if exists public.active_tenant_trainer_user_ids(bigint);
create or replace function public.active_tenant_trainer_user_ids(in_tenant_id bigint)
returns setof bigint
language sql
stable
as $$
  select distinct u.u_id
  from public.tenant_trainer tt
  join public.user_proxy up on up.person_id = tt.person_id and up.active
  join public.users u on u.u_id = up.user_id
  where tt.tenant_id = in_tenant_id
    and tt.active
    and u.tenant_id = in_tenant_id;
$$;

drop function if exists public.active_tenant_administrator_user_ids(bigint);
create or replace function public.active_tenant_administrator_user_ids(in_tenant_id bigint)
returns setof bigint
language sql
stable
as $$
  select distinct u.u_id
  from public.tenant_administrator ta
  join public.user_proxy up on up.person_id = ta.person_id and up.active
  join public.users u on u.u_id = up.user_id
  where ta.tenant_id = in_tenant_id
    and ta.active
    and u.tenant_id = in_tenant_id;
$$;

