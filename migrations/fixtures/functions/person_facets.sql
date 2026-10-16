drop function app_private.person_membership_candidates;
drop function person_facets;

create or replace function app_private.person_membership_candidates(membership_state text)
returns table(person_id bigint, in_state boolean, cohort_ids bigint[], is_trainer boolean, is_admin boolean)
language sql stable as $$
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
revoke all on function app_private.person_membership_candidates from public;
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
revoke all on function person_facets from public;
grant execute on function person_facets to anonymous;
