create or replace function federated.federation_competitor_facets(
  federation federated.federation,
  search text default null,
  competitor_types federated.competitor_type[] default null,
  age_groups text[] default null,
  classes text[] default null,
  disciplines text[] default null,
  competed_since date default null
) returns table(facet text, value text, count integer)
language sql stable as $$
  with candidates as materialized (
    select c.id, c.competitor_type::text as competitor_type,
      p.age_group_code, p.class, p.discipline,
      coalesce(cardinality(competitor_types), 0) = 0 or c.competitor_type = any(competitor_types) as type_matches,
      -- Missing progress matches only category filters that are absent.
      coalesce(p.age_matches, coalesce(cardinality(age_groups), 0) = 0) as age_matches,
      coalesce(p.class_matches, coalesce(cardinality(classes), 0) = 0) as class_matches,
      coalesce(p.discipline_matches, coalesce(cardinality(disciplines), 0) = 0) as discipline_matches
    from federated.competitor c
    left join app_private.competitor_category_candidates(age_groups, classes, disciplines) p
      on p.competitor_id = c.id
    where c.federation = federation.code
      and (nullif(trim(search), '') is null or c.external_id::text = trim(search) or c.id = trim(search)
        or c.search_name like '%' || replace(replace(replace(
          app_private.normalize_name(trim(search)), '!', '!!'), '%', '!%'), '_', '!_') || '%' escape '!')
      and (competed_since is null or (c.latest_result_at >= competed_since
        and (c.latest_result_at <= current_date or exists (
          select 1 from federated.competition_result r
          join federated.competition comp on comp.id = r.competition_id
          where r.competitor_id = c.id and comp.start_date >= competed_since and comp.start_date <= current_date
        ))))
  )
  select option.facet, option.value, count(distinct c.id)::integer
  from candidates c
  cross join lateral (values
    ('type', c.competitor_type, c.age_matches and c.class_matches and c.discipline_matches),
    ('ageGroup', c.age_group_code, c.type_matches and c.class_matches and c.discipline_matches),
    ('class', c.class, c.type_matches and c.age_matches and c.discipline_matches),
    ('discipline', c.discipline, c.type_matches and c.age_matches and c.class_matches)
  ) option(facet, value, matches)
  where option.matches and option.value is not null
  group by option.facet, option.value
  order by option.facet, option.value;
$$;

comment on function federated.federation_competitor_facets is '@name federation_competitor_facets
@behavior +list -connection';
revoke all on function federated.federation_competitor_facets from public;
grant execute on function federated.federation_competitor_facets to anonymous;
