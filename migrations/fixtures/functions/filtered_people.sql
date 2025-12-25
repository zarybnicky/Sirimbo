CREATE or replace FUNCTION public.filtered_people(
  is_trainer boolean,
  is_admin boolean,
  in_cohorts bigint[] default null,
  membership_state text default 'current'
) RETURNS SETOF person LANGUAGE sql STABLE AS $$
  select p.* from (
    select tm.person_id from tenant_membership tm
    where lower(coalesce(membership_state,'current')) = 'former' and tm.tenant_id = (select current_tenant_id()) and tm.status = 'expired'
      and not exists(select 1 from tenant_membership a where a.person_id = tm.person_id and a.tenant_id = tm.tenant_id and a.status = 'active')
    union
    select tm.person_id from tenant_membership tm
    where lower(coalesce(membership_state,'current')) = 'current' and tm.tenant_id = (select current_tenant_id()) and tm.status = 'active'
    union
    select tt.person_id from tenant_trainer tt
    where lower(coalesce(membership_state,'current')) = 'current' and tt.tenant_id = (select current_tenant_id()) and tt.status = 'active'
    union
    select ta.person_id from tenant_administrator ta
    where lower(coalesce(membership_state,'current')) = 'current' and ta.tenant_id = (select current_tenant_id()) and ta.status = 'active'
  ) vis
  join person p on p.id=vis.person_id
  where (
    lower(coalesce(membership_state,'current')) = 'former' and (
      in_cohorts is null or (
        cardinality(in_cohorts) = 0 and not exists(
          select 1 from cohort_membership cm where cm.person_id = p.id and cm.tenant_id = (select current_tenant_id()) and cm.status = 'expired')
      ) or (
        cardinality(in_cohorts) > 0 and exists(
          select 1 from cohort_membership cm where cm.person_id = p.id and cm.tenant_id = (select current_tenant_id()) and cm.status = 'expired' and cm.cohort_id = any(in_cohorts))
      )
    ) and (
      is_trainer is null or is_trainer = exists(
        select 1 from tenant_trainer tt where tt.person_id = p.id and tt.tenant_id = (select current_tenant_id()) and tt.status = 'expired')
    ) and (
      is_admin is null or is_admin = exists(
        select 1 from tenant_administrator ta where ta.person_id = p.id and ta.tenant_id = (select current_tenant_id()) and ta.status = 'expired')
    )
  ) or (
    lower(coalesce(membership_state,'current')) = 'current' and (
      in_cohorts is null or (
        cardinality(in_cohorts) = 0 and not exists(
          select 1 from cohort_membership cm where cm.person_id = p.id and cm.tenant_id = (select current_tenant_id()) and cm.status = 'active')
      ) or (
        cardinality(in_cohorts) > 0 and exists(
          select 1 from cohort_membership cm where cm.person_id = p.id and cm.tenant_id = (select current_tenant_id()) and cm.status = 'active' and cm.cohort_id = any(in_cohorts))
      )
    ) and (
      is_trainer is null or is_trainer = exists(
        select 1 from tenant_trainer tt where tt.person_id = p.id and tt.tenant_id = (select current_tenant_id()) and tt.status = 'active')
    ) and (
      is_admin is null or is_admin = exists(
        select 1 from tenant_administrator ta where ta.person_id = p.id and ta.tenant_id = (select current_tenant_id()) and ta.status = 'active'))
    );
$$;
COMMENT ON FUNCTION public.filtered_people IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.filtered_people TO anonymous;
