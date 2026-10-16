CREATE FUNCTION public.person_facets(membership_states text[] DEFAULT ARRAY['current'::text], in_cohorts bigint[] DEFAULT NULL::bigint[], is_trainer boolean DEFAULT NULL::boolean, is_admin boolean DEFAULT NULL::boolean) RETURNS TABLE(facet text, value text, count integer)
    LANGUAGE sql STABLE
    AS $_$
  with candidates as (
    select s.state, m.*,
      in_cohorts is null or case when cardinality(in_cohorts) = 0
        then cardinality(m.cohort_ids) = 0 else m.cohort_ids && in_cohorts end as cohort_matches,
       $3 is null or m.is_trainer = $3 as trainer_matches,
       $4 is null or m.is_admin = $4 as admin_matches
    from (values ('current'), ('former')) s(state)
    cross join lateral app_private.person_membership_candidates(s.state) m
    where m.in_state
  )
  select option.facet, option.value, count(distinct c.person_id)::integer
  from candidates c
  cross join lateral (
    select 'cohort' as facet, unnest(case when cardinality(c.cohort_ids) = 0
      then array['none'] else c.cohort_ids::text[] end) as value
    where (coalesce(cardinality(membership_states), 0) = 0 or c.state = any(membership_states)) and c.trainer_matches and c.admin_matches
    union all
    select 'trainer', c.is_trainer::text
    where (coalesce(cardinality(membership_states), 0) = 0 or c.state = any(membership_states)) and c.cohort_matches and c.admin_matches
    union all
    select 'admin', c.is_admin::text
    where (coalesce(cardinality(membership_states), 0) = 0 or c.state = any(membership_states)) and c.cohort_matches and c.trainer_matches
    union all
    select 'state', c.state
    where c.cohort_matches and c.trainer_matches and c.admin_matches
  ) option
  group by option.facet, option.value
  order by option.facet, option.value;
$_$;

COMMENT ON FUNCTION public.person_facets(membership_states text[], in_cohorts bigint[], is_trainer boolean, is_admin boolean) IS '@behavior +list -connection';

REVOKE ALL ON FUNCTION public.person_facets(membership_states text[], in_cohorts bigint[], is_trainer boolean, is_admin boolean) FROM PUBLIC;
GRANT ALL ON FUNCTION public.person_facets(membership_states text[], in_cohorts bigint[], is_trainer boolean, is_admin boolean) TO anonymous;
