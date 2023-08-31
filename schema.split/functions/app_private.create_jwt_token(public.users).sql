CREATE FUNCTION app_private.create_jwt_token(u public.users) RETURNS public.jwt_token
    LANGUAGE sql STABLE
    AS $$
  with details as (
    SELECT
      user_id,
      person.id as person_id,
      (select array_agg(tenant_id) from tenant_membership where person_id=person.id) as my_tenant_ids,
      (select array_agg(cohort_id) from cohort_membership where person_id=person.id) as my_cohort_ids,
      (select array_agg(id) from couple where man_id=person.id or woman_id=person.id) as my_couple_ids,
      exists (select 1 from tenant_membership where tenant_membership.person_id=person.id and tenant_membership.tenant_id = current_tenant_id() and now() <@ active_range) as is_member,
      exists (select 1 from tenant_trainer where tenant_trainer.person_id=person.id and tenant_trainer.tenant_id = current_tenant_id() and now() <@ active_range) as is_trainer,
      exists (select 1 from tenant_administrator where tenant_administrator.person_id=person.id and tenant_administrator.tenant_id = current_tenant_id() and now() <@ active_range) as is_admin
    from user_proxy join person on person_id=person.id where user_id=u.u_id
  ) select
    extract(epoch from now() + interval '7 days')::integer,
    u.u_id,
    current_tenant_id(),
    u.u_login,
    u.u_email,
    array_agg(person_id) as my_person_ids,
    array_accum(my_tenant_ids) as my_tenant_ids,
    array_accum(my_cohort_ids) as my_cohort_ids,
    array_accum(my_couple_ids) as my_couple_ids,
    bool_or(is_member) as is_member,
    bool_or(is_trainer) as is_trainer,
    bool_or(is_admin) as is_admin
  from details group by user_id;
$$;



