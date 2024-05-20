CREATE FUNCTION public.current_user_id() RETURNS bigint
    LANGUAGE sql STABLE
    AS $$
  SELECT nullif(current_setting('jwt.claims.user_id', true), '')::bigint;
$$;

COMMENT ON FUNCTION public.current_user_id() IS '@omit';

GRANT ALL ON FUNCTION public.current_user_id() TO anonymous;


