create or replace function federated.federation_people_facets(
  federation federated.federation,
  search text default null,
  age_groups text[] default null,
  genders text[] default null,
  nationalities text[] default null
) returns table(facet text, value text, count integer)
language sql stable as $$
  with candidates as (
    select
      coalesce(p.age_group_code, nullif(p.age_group, ''), 'unknown') as age_group,
      coalesce(p.gender::text, 'unknown') as gender,
      coalesce(nullif(p.nationality, ''), 'unknown') as nationality
    from federated.person p
    where p.federation = federation.code
      and (nullif(trim(search), '') is null or p.external_id::text = trim(search) or p.id = trim(search)
        or p.search_name like '%' || replace(replace(replace(
          app_private.normalize_name(trim(search)), '!', '!!'), '%', '!%'), '_', '!_') || '%' escape '!')
  ), matches as (
    select *,
      coalesce(cardinality(age_groups), 0) = 0 or age_group = any(age_groups) as age_matches,
      coalesce(cardinality(genders), 0) = 0 or gender = any(genders) as gender_matches,
      coalesce(cardinality(nationalities), 0) = 0 or nationality = any(nationalities) as nationality_matches
    from candidates
  )
  select option.facet, option.value, count(*)::integer
  from matches m
  cross join lateral (values
    ('ageGroup', m.age_group, m.gender_matches and m.nationality_matches),
    ('gender', m.gender, m.age_matches and m.nationality_matches),
    ('nationality', m.nationality, m.age_matches and m.gender_matches)
  ) option(facet, value, matches)
  where option.matches
  group by option.facet, option.value
  order by option.facet, option.value;
$$;

comment on function federated.federation_people_facets is '@name federation_people_facets
@behavior +list -connection';
revoke all on function federated.federation_people_facets from public;
grant execute on function federated.federation_people_facets to anonymous;
