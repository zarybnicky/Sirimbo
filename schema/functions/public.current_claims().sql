CREATE FUNCTION public.current_claims() RETURNS jsonb
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  select to_jsonb(app_private.create_jwt_token(users)) - 'exp'
  from users
  where id = nullif(current_setting('jwt.claims.user_id', true), '')::bigint;
$$;

GRANT ALL ON FUNCTION public.current_claims() TO anonymous;
