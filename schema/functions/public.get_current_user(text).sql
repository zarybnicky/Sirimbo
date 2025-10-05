CREATE FUNCTION public.get_current_user(version_id text DEFAULT NULL::text) RETURNS public.users
    LANGUAGE sql SECURITY DEFINER
    AS $$
  with updated_user as (
    update users
    set
      last_active_at = now(),
      last_version = version_id
    where u_id = nullif(current_setting('jwt.claims.user_id', true), '')::integer
    returning *
  )
  select * from updated_user
  union all
  select *
  from users
  where u_id = nullif(current_setting('jwt.claims.user_id', true), '')::integer
    and not exists (select 1 from updated_user);
$$;

GRANT ALL ON FUNCTION public.get_current_user(version_id text) TO anonymous;
