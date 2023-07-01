CREATE FUNCTION public.current_permissions() RETURNS SETOF public.permissions
    LANGUAGE sql STABLE
    AS $$
  SELECT permissions.* from permissions
  inner join users on u_group=pe_id
  where u_id=current_setting('jwt.claims.user_id', true)::bigint;
$$;



