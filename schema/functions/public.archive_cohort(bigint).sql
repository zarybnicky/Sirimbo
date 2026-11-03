CREATE FUNCTION public.archive_cohort(cohort_id bigint) RETURNS public.cohort
    LANGUAGE sql
    AS $_$
  update cohort_membership
  set until = now(), status = 'expired'
  where cohort_id = $1
    and since < now()
    and (until is null or until > now());

  update cohort set is_visible = false where id = $1 returning *;
$_$;

GRANT ALL ON FUNCTION public.archive_cohort(cohort_id bigint) TO administrator;
