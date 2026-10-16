CREATE FUNCTION federated.federation_competitor_facets(federation federated.federation, search text DEFAULT NULL::text, competitor_types federated.competitor_type[] DEFAULT NULL::federated.competitor_type[], age_groups text[] DEFAULT NULL::text[], classes text[] DEFAULT NULL::text[], disciplines text[] DEFAULT NULL::text[], competed_since date DEFAULT NULL::date) RETURNS TABLE(facet text, value text, count integer)
    LANGUAGE sql STABLE
    AS $$
  with candidates as materialized (
    select c.id, c.competitor_type::text as competitor_type,
      cat.age_group_code, cat.class, cat.discipline,
      coalesce(cardinality(competitor_types), 0) = 0 or c.competitor_type = any(competitor_types) as type_matches,
      coalesce(cardinality(age_groups), 0) = 0 or cat.age_group_code = any(
        array(select federated.resolve_age_group(value) from unnest(age_groups) value)
      ) as age_matches,
      coalesce(cardinality(classes), 0) = 0 or cat.class = any(classes) as class_matches,
      coalesce(cardinality(disciplines), 0) = 0 or cat.discipline = any(disciplines) as discipline_matches
    from federated.competitor c
    left join federated.competitor_category_progress p on p.competitor_id = c.id
    left join federated.category cat on cat.id = p.category_id
    where c.federation = federation.code
      and (nullif(trim(search), '') is null or c.external_id::text = trim(search) or c.id = trim(search)
        or c.search_name like '%' || replace(replace(replace(
          app_private.normalize_name(trim(search)), '!', '!!'), '%', '!%'), '_', '!_') || '%' escape '!')
      and (competed_since is null or exists (
        select 1 from federated.competition_result r
        join federated.competition comp on comp.id = r.competition_id
        where r.competitor_id = c.id and comp.start_date >= competed_since and comp.start_date <= current_date
      ))
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

COMMENT ON FUNCTION federated.federation_competitor_facets(federation federated.federation, search text, competitor_types federated.competitor_type[], age_groups text[], classes text[], disciplines text[], competed_since date) IS '@behavior +list -connection';

REVOKE ALL ON FUNCTION federated.federation_competitor_facets(federation federated.federation, search text, competitor_types federated.competitor_type[], age_groups text[], classes text[], disciplines text[], competed_since date) FROM PUBLIC;
GRANT ALL ON FUNCTION federated.federation_competitor_facets(federation federated.federation, search text, competitor_types federated.competitor_type[], age_groups text[], classes text[], disciplines text[], competed_since date) TO anonymous;
