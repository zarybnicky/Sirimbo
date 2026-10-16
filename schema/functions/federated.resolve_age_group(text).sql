CREATE FUNCTION federated.resolve_age_group(source_name text) RETURNS text
    LANGUAGE sql STABLE STRICT
    AS $$
  select code from federated.age_group
  where regexp_replace(lower(source_name), '[^a-z0-9]', '', 'g')
    = any(array_prepend(regexp_replace(lower(code), '[^a-z0-9]', '', 'g'), aliases));
$$;

COMMENT ON FUNCTION federated.resolve_age_group(source_name text) IS '@behavior -*';

REVOKE ALL ON FUNCTION federated.resolve_age_group(source_name text) FROM PUBLIC;
GRANT ALL ON FUNCTION federated.resolve_age_group(source_name text) TO anonymous;
