CREATE FUNCTION public.my_persons_array() RETURNS bigint[]
    LANGUAGE sql STABLE
    AS $$
  SELECT translate(nullif(current_setting('jwt.claims.my_person_ids', true), ''), '[]', '{}')::bigint[];
$$;

COMMENT ON FUNCTION public.my_persons_array() IS '@omit';

GRANT ALL ON FUNCTION public.my_persons_array() TO anonymous;


