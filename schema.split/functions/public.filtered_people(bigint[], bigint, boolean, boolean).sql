CREATE FUNCTION public.filtered_people(in_tenants bigint[], in_cohort bigint, is_trainer boolean, is_admin boolean) RETURNS SETOF public.person
    LANGUAGE sql STABLE
    AS $$
 select person.* from person
  where
    exists (select 1 from tenant_membership where tenant_id = any (in_tenants) and person_id=person.id and active=true)
  and
    case when in_cohort is null then true
    else exists (select 1 from cohort_membership where cohort_id=in_cohort and person_id=person.id and active=true) end
  and
    case when is_trainer = false then true
    else exists (select 1 from tenant_trainer where tenant_id = any (in_tenants) and person_id=person.id) end
  and
    case when is_admin = false then true
    else exists (select 1 from tenant_administrator where tenant_id = any (in_tenants) and person_id=person.id) end
$$;

COMMENT ON FUNCTION public.filtered_people(in_tenants bigint[], in_cohort bigint, is_trainer boolean, is_admin boolean) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.filtered_people(in_tenants bigint[], in_cohort bigint, is_trainer boolean, is_admin boolean) TO anonymous;


