create or replace function app_private.create_jwt_token(u users) returns jwt_token
    language sql stable
as $$
with
  person_ids as (
    select distinct up.person_id from user_proxy up where up.user_id = u.id and up.status = 'active'
  ),
  tenant_claims as (
    select
      coalesce(array_agg(distinct tenant_id), '{}'::bigint[]) as tenant_ids,
      coalesce(array_agg(distinct tenant_id) filter (where 'member' = any(roles)), '{}'::bigint[]) as member_ids,
      coalesce(array_agg(distinct tenant_id) filter (where 'trainer' = any(roles)), '{}'::bigint[]) as trainer_ids,
      coalesce(array_agg(distinct tenant_id) filter (where 'administrator' = any(roles)), '{}'::bigint[]) as admin_ids
    from (
      select r.tenant_id, r.current_roles as roles
      from person_ids p join app_private.tenant_relation r on r.person_id = p.person_id
      where r.is_current
      union all
      select id, array['administrator']::public.tenant_role[]
      from tenant where app_private.is_system_admin(u.id)
    ) roles
  ),
  cohort_ids as (
    select distinct unnest(r.current_cohort_ids) as cohort_id
    from app_private.tenant_relation r join person_ids p on p.person_id = r.person_id
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
    u.u_email as email,

    coalesce((select array_agg(p.person_id) from person_ids p), '{}'::bigint[]) as my_person_ids,
    tc.tenant_ids as my_tenant_ids,
    coalesce((select array_agg(p.cohort_id) from cohort_ids p), '{}'::bigint[]) as my_cohort_ids,
    coalesce((select array_agg(p.id)        from couple_ids p), '{}'::bigint[]) as my_couple_ids,

    app_private.is_system_admin(u.id) as is_system_admin,

    '{}'::bigint[] as guest_tenant_ids,
    tc.member_ids as member_tenant_ids,
    tc.trainer_ids as trainer_tenant_ids,
    tc.admin_ids as admin_tenant_ids
  from tenant_claims tc;
$$;
