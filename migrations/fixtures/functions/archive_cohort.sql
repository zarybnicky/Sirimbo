create or replace function archive_cohort(cohort_id bigint)
  returns cohort
  language sql
as $$
  update cohort_membership
  set until = now(), status = 'expired'
  where cohort_id = $1
    and since < now()
    and (until is null or until > now());

  update cohort set is_visible = false where id = $1 returning *;
$$;

revoke all on function archive_cohort from anonymous;
grant all on function archive_cohort to administrator;
