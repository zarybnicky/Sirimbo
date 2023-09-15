CREATE FUNCTION public.my_couples_array() RETURNS bigint[]
    LANGUAGE sql STABLE
    AS $$
  SELECT translate(nullif(current_setting('jwt.claims.my_couple_ids', true), ''), '[]', '{}')::bigint[];
$$;

COMMENT ON FUNCTION public.my_couples_array() IS '@omit';

GRANT ALL ON FUNCTION public.my_couples_array() TO anonymous;


