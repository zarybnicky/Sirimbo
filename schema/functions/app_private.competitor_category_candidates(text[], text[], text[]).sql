CREATE FUNCTION app_private.competitor_category_candidates(age_groups text[] DEFAULT NULL::text[], classes text[] DEFAULT NULL::text[], disciplines text[] DEFAULT NULL::text[]) RETURNS TABLE(competitor_id text, age_group_code text, class text, discipline text, age_matches boolean, class_matches boolean, discipline_matches boolean)
    LANGUAGE sql STABLE
    AS $$
  select p.competitor_id, cat.age_group_code, cat.class, cat.discipline,
    coalesce(cardinality(age_groups), 0) = 0 or cat.age_group_code = any(
      array(select federated.resolve_age_group(value) from unnest(age_groups) value)
    ),
    coalesce(cardinality(classes), 0) = 0 or cat.class = any(classes),
    coalesce(cardinality(disciplines), 0) = 0 or cat.discipline = any(disciplines)
  from federated.competitor_category_progress p
  join federated.category cat on cat.id = p.category_id;
$$;

GRANT ALL ON FUNCTION app_private.competitor_category_candidates(age_groups text[], classes text[], disciplines text[]) TO anonymous;
