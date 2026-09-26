CREATE FUNCTION public.log_in_as(id bigint) RETURNS public.login_result
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $_$
declare
  v_id alias for $1;
  usr users;
  jwt jwt_token;
begin
  select * into strict usr from users where users.id = v_id;
  jwt := app_private.create_jwt_token(usr);
  insert into security_event (user_id, kind, method)
  values (usr.id, 'impersonation', 'manual');
  return (usr, jwt);
end;
$_$;

GRANT ALL ON FUNCTION public.log_in_as(id bigint) TO administrator;
