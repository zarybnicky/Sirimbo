CREATE FUNCTION public.my_couple_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE ROWS 5
    AS $$
  SELECT json_array_elements_text(nullif(current_setting('jwt.claims.my_couple_ids', true), '')::json)::bigint;
$$;

COMMENT ON FUNCTION public.my_couple_ids() IS '@omit';

GRANT ALL ON FUNCTION public.my_couple_ids() TO anonymous;
