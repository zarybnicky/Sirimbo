CREATE FUNCTION public.current_person_ids() RETURNS bigint[]
    LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE
    AS $$
  SELECT translate(nullif(current_setting('jwt.claims.my_person_ids', true), '[]'), '[]', '{}')::bigint[] || array[]::bigint[];
$$;

COMMENT ON FUNCTION public.current_person_ids() IS '@omit';

GRANT ALL ON FUNCTION public.current_person_ids() TO anonymous;
