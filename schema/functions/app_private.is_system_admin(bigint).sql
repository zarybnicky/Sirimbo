CREATE FUNCTION app_private.is_system_admin(bigint) RETURNS boolean
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'app_private', 'public', 'pg_temp'
    AS $_$
  select coalesce(exists(
    select 1 from app_private.system_admin_user sau where user_id = $1
  ), false);
$_$;

COMMENT ON FUNCTION app_private.is_system_admin(bigint) IS 'Returns true when the given user id has global system administrator privileges.';

GRANT ALL ON FUNCTION app_private.is_system_admin(bigint) TO anonymous;
