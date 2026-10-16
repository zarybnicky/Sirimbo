CREATE FUNCTION app_private.person_membership_candidates(membership_state text) RETURNS TABLE(person_id bigint, in_state boolean, cohort_ids bigint[], is_trainer boolean, is_admin boolean)
    LANGUAGE sql STABLE
    AS $$
  with current_people as (
    select person_id from current_tenant_membership
    union select person_id from current_tenant_trainer
    union select person_id from current_tenant_administrator
  ), former_people as (
    select person_id from tenant_membership where tenant_id = current_tenant_id() and status = 'expired'
    union select person_id from tenant_trainer where tenant_id = current_tenant_id() and status = 'expired'
    union select person_id from tenant_administrator where tenant_id = current_tenant_id() and status = 'expired'
  ), trainers as (
    select person_id from current_tenant_trainer where membership_state = 'current'
    union all
    select person_id from tenant_trainer
    where membership_state = 'former' and tenant_id = current_tenant_id() and status = 'expired'
  ), admins as (
    select person_id from current_tenant_administrator where membership_state = 'current'
    union all
    select person_id from tenant_administrator
    where membership_state = 'former' and tenant_id = current_tenant_id() and status = 'expired'
  ), cohorts as (
    select person_id, cohort_id from current_cohort_membership where membership_state = 'current'
    union all
    select person_id, cohort_id from cohort_membership
    where membership_state = 'former' and tenant_id = current_tenant_id() and status = 'expired'
  )
  select p.id,
    case membership_state
      when 'current' then p.id in (select person_id from current_people)
      when 'former' then p.id in (select person_id from former_people)
        and p.id not in (select person_id from current_people)
      else false
    end,
    array(select distinct cohort_id from cohorts where person_id = p.id),
    p.id in (select person_id from trainers),
    p.id in (select person_id from admins)
  from person p;
$$;

REVOKE ALL ON FUNCTION app_private.person_membership_candidates(membership_state text) FROM PUBLIC;
GRANT ALL ON FUNCTION app_private.person_membership_candidates(membership_state text) TO anonymous;
