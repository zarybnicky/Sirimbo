
--!include functions/create_latest_lesson_payments.sql
--!include functions/tg_account_balances__update.sql
--!include functions/tg_cohort_membership__on_status.sql

CREATE OR REPLACE FUNCTION public.sync_cohort_memberships(person_id bigint, cohort_ids bigint[]) RETURNS void
    LANGUAGE sql
    AS $_$
  update cohort_membership set until = now(), status = 'expired'
  where status = 'active' and person_id = $1 and cohort_id <> all (cohort_ids);

  insert into cohort_membership (status, since, person_id, cohort_id)
  select 'active', NOW(), $1, new_cohort_id
  from unnest(cohort_ids) as x(new_cohort_id)
  where not exists (select 1 from cohort_membership where status = 'active' and person_id = $1 and cohort_id = new_cohort_id);
$_$;

GRANT ALL ON FUNCTION public.sync_cohort_memberships(person_id bigint, cohort_ids bigint[]) TO administrator;
