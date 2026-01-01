CREATE FUNCTION public.log_in_as(id bigint) RETURNS public.login_result
    LANGUAGE sql STRICT SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $_$
  select (
    (select (users.*)::users from users where users.id = $1),
    (select app_private.create_jwt_token(users.*) from users where users.id = $1)
  );
$_$;

GRANT ALL ON FUNCTION public.log_in_as(id bigint) TO administrator;
