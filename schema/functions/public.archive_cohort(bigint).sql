CREATE FUNCTION public.archive_cohort(cohort_id bigint) RETURNS public.cohort
    LANGUAGE sql
    AS $_$
  update cohort_membership set until=now() where cohort_id = $1;
  update cohort
  set is_visible = false, cohort_group_id = null
  where id = $1
  returning *;
$_$;

GRANT ALL ON FUNCTION public.archive_cohort(cohort_id bigint) TO administrator;
