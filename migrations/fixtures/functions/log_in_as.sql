drop function if exists app_private.log_in_as;
drop function if exists log_in_as;

CREATE FUNCTION public.log_in_as(id bigint) RETURNS login_result
  LANGUAGE sql STRICT SECURITY DEFINER
  SET search_path TO pg_catalog, public, pg_temp
AS $$
  select (
    (select (users.*)::users from users where users.id = $1),
    (select app_private.create_jwt_token(users.*) from users where users.id = $1)
  );
$$;

GRANT ALL ON FUNCTION public.log_in_as TO administrator;
