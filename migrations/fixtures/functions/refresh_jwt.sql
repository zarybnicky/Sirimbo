CREATE or replace FUNCTION refresh_jwt() RETURNS jwt_token
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  SELECT app_private.create_jwt_token(users) FROM users WHERE id = nullif(current_setting('jwt.claims.user_id', true), '')::integer;
$$;

GRANT ALL ON FUNCTION public.refresh_jwt() TO anonymous;
