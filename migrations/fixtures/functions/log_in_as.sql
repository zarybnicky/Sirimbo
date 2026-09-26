drop function if exists log_in_as;

CREATE FUNCTION log_in_as(id bigint) RETURNS login_result
  LANGUAGE plpgsql
  STRICT SECURITY DEFINER
  SET search_path TO pg_catalog, public, pg_temp
AS $$
declare
  v_id alias for $1;
  usr users;
  jwt jwt_token;
begin
  select * into strict usr from users where users.id = v_id;
  jwt := app_private.create_jwt_token(usr);
  perform app_private.add_security_event('impersonation', 'manual', usr.id);
  return (usr, jwt);
end;
$$;

GRANT ALL ON FUNCTION log_in_as TO administrator;
