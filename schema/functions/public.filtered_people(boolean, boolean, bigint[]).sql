CREATE FUNCTION public.filtered_people(is_trainer boolean, is_admin boolean, in_cohorts bigint[] DEFAULT NULL::bigint[]) RETURNS SETOF public.person
    LANGUAGE sql STABLE
    AS $$
  select person.* from person
  join auth_details on person_id=person.id
  where
    current_tenant_id() = any (auth_details.allowed_tenants)
    and case when in_cohorts is null then true else in_cohorts = auth_details.cohort_memberships OR in_cohorts && auth_details.cohort_memberships end
    and case when is_trainer is null then true else is_trainer = (current_tenant_id() = any (auth_details.tenant_trainers)) end
    and case when is_admin is null then true else is_admin = (current_tenant_id() = any (auth_details.tenant_administrators)) end
  order by last_name, first_name
$$;

COMMENT ON FUNCTION public.filtered_people(is_trainer boolean, is_admin boolean, in_cohorts bigint[]) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.filtered_people(is_trainer boolean, is_admin boolean, in_cohorts bigint[]) TO anonymous;
