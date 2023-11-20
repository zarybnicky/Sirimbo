CREATE FUNCTION public.current_session_id() RETURNS text
    LANGUAGE sql STABLE
    AS $$
  select nullif(current_setting('jwt.claims.user_id', true), '')::integer;
$$;

GRANT ALL ON FUNCTION public.current_session_id() TO anonymous;


