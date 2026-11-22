drop function if exists app_private.person_membership_candidates;
drop function if exists person_facets;

create or replace function app_private.person_membership_candidates(membership_state text)
returns table(person_id bigint, in_state boolean, cohort_ids bigint[], is_trainer boolean, is_admin boolean)
language sql stable as $$
  select p.id,
    coalesce(case membership_state when 'current' then r.is_current when 'former' then r.is_former end, false),
    coalesce(case membership_state when 'current' then r.current_cohort_ids when 'former' then r.expired_cohort_ids end, '{}'),
    coalesce('trainer' = any(case membership_state when 'current' then r.current_roles when 'former' then r.expired_roles end), false),
    coalesce('administrator' = any(case membership_state when 'current' then r.current_roles when 'former' then r.expired_roles end), false)
  from person p
  left join app_private.tenant_relation r on r.person_id = p.id and r.tenant_id = current_tenant_id();
$$;
grant execute on function app_private.person_membership_candidates to anonymous;

create or replace function person_facets(
  membership_states text[] default array['current'],
  in_cohorts bigint[] default null,
  is_trainer boolean default null,
  is_admin boolean default null
) returns table(facet text, value text, count integer)
language sql stable as $$
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
$$;
comment on function person_facets is '@behavior +list -connection';
grant execute on function person_facets to anonymous;
