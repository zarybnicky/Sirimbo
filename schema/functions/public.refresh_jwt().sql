CREATE FUNCTION public.refresh_jwt() RETURNS public.jwt_token
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select app_private.create_jwt_token(users)
  from users
  where id = current_user_id();
$$;

GRANT ALL ON FUNCTION public.refresh_jwt() TO anonymous;
