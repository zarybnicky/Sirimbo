CREATE FUNCTION public.get_current_user(version_id text DEFAULT NULL::text) RETURNS public.users
    LANGUAGE sql VOLATILE SECURITY DEFINER
    AS $$
  WITH updated_user AS (
    UPDATE public.users
    SET last_active_at = now(), last_version = version_id
    WHERE u_id = nullif(current_setting('jwt.claims.user_id', true), '')::integer
    RETURNING *
  )
  SELECT * FROM updated_user
  UNION ALL
  SELECT *
  FROM public.users
  WHERE u_id = nullif(current_setting('jwt.claims.user_id', true), '')::integer
    AND NOT EXISTS (SELECT 1 FROM updated_user);
$$;

GRANT ALL ON FUNCTION public.get_current_user(text) TO anonymous;
