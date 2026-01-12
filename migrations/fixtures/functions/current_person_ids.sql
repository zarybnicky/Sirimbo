CREATE or replace FUNCTION current_person_ids() RETURNS bigint[] LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE AS $$
  SELECT translate(coalesce(nullif(current_setting('jwt.claims.my_person_ids', true), ''), '{}'), '[]', '{}')::bigint[];
$$;

COMMENT ON FUNCTION current_person_ids() IS '@omit';
GRANT ALL ON FUNCTION current_person_ids() TO anonymous;

CREATE or replace FUNCTION current_couple_ids() RETURNS bigint[] LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE AS $$
  SELECT translate(coalesce(nullif(current_setting('jwt.claims.my_couple_ids', true), ''), '{}'), '[]', '{}')::bigint[];
$$;

COMMENT ON FUNCTION current_couple_ids() IS '@omit';
GRANT ALL ON FUNCTION current_couple_ids() TO anonymous;

CREATE or replace FUNCTION my_tenants_array() RETURNS bigint[] LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE AS $$
  SELECT translate(coalesce(nullif(current_setting('jwt.claims.my_tenant_ids', true), ''), '{}'), '[]', '{}')::bigint[];
$$;
COMMENT ON FUNCTION my_tenants_array() IS '@omit';
GRANT ALL ON FUNCTION my_tenants_array() TO anonymous;
