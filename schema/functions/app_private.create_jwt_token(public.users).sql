CREATE FUNCTION app_private.create_jwt_token(u public.users) RETURNS public.jwt_token
    LANGUAGE sql STABLE
    AS $$
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

COMMENT ON FUNCTION app_private.create_jwt_token(u public.users) IS 'Generates the JWT payload including global system administrator flag.';
