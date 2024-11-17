CREATE FUNCTION public.refresh_jwt() RETURNS public.jwt_token
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  SELECT app_private.create_jwt_token(users) FROM users WHERE u_id = nullif(current_setting('jwt.claims.user_id', true), '')::integer;
$$;

GRANT ALL ON FUNCTION public.refresh_jwt() TO anonymous;
