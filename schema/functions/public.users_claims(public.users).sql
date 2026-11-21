CREATE FUNCTION public.users_claims(u public.users) RETURNS jsonb
    LANGUAGE sql STABLE STRICT SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select to_jsonb(app_private.create_jwt_token(actual_user)) - 'exp' - 'tenant_id'
  from users actual_user
  where actual_user.id = u.id
    and (u.id = current_user_id()
      or pg_has_role(current_setting('role'), 'administrator', 'member'));
$$;

COMMENT ON FUNCTION public.users_claims(u public.users) IS '@behavior +typeField -queryField';

GRANT ALL ON FUNCTION public.users_claims(u public.users) TO anonymous;
