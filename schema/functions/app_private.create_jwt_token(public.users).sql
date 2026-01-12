CREATE FUNCTION app_private.create_jwt_token(u public.users) RETURNS public.jwt_token
    LANGUAGE sql STABLE
    AS $$
with
  person_ids as (
    select distinct up.person_id from user_proxy up where up.user_id = u.id
  ),
  tenant_memberships as (
    select distinct tm.tenant_id from tenant_membership tm join person_ids p on p.person_id = tm.person_id where tm.status = 'active'
  ),
  tenant_trainers as (
    select distinct tt.tenant_id from tenant_trainer tt join person_ids p on p.person_id = tt.person_id where tt.status = 'active'
  ),
  tenant_admins as (
    select distinct ta.tenant_id from tenant_administrator ta join person_ids p on p.person_id = ta.person_id where ta.status = 'active'
    union
    select id from tenant where app_private.is_system_admin(u.id)
  ),
  tenant_ids as (
    select tenant_id from tenant_memberships
    union
    select tenant_id from tenant_trainers
    union
    select tenant_id from tenant_admins
  ),
  cohort_ids as (
    select distinct cm.cohort_id from cohort_membership cm join person_ids p on p.person_id = cm.person_id where cm.status = 'active'
  ),
  couple_ids as (
    select distinct c.id from couple c join person_ids p on p.person_id = c.man_id where c.status = 'active'
    union all
    select distinct c.id from couple c join person_ids p on p.person_id = c.woman_id where c.status = 'active'
  )
  select
    extract(epoch from now() + interval '7 days')::integer as exp,
    u.id as user_id,
    (select current_tenant_id()) as tenant_id,
    u.u_login as username,
    u.u_email as email,
    coalesce((select array_agg(p.person_id) from person_ids p), '{}'::bigint[]) as my_person_ids,
    coalesce((select array_agg(p.tenant_id) from tenant_ids p), '{}'::bigint[]) as my_person_ids,
    coalesce((select array_agg(p.cohort_id) from cohort_ids p), '{}'::bigint[]) as my_person_ids,
    coalesce((select array_agg(p.id) from couple_ids p), '{}'::bigint[]) as my_person_ids,
    exists (select 1 from tenant_ids t where t.tenant_id = (select current_tenant_id())) as is_member,
    exists (select 1 from tenant_trainers t where t.tenant_id = (select current_tenant_id())) as is_trainer,
    exists (select 1 from tenant_admins a where a.tenant_id = (select current_tenant_id())) as is_admin,
    app_private.is_system_admin(u.id) as is_system_admin;
$$;

COMMENT ON FUNCTION app_private.create_jwt_token(u public.users) IS 'Generates the JWT payload including global system administrator flag.';
