drop function if exists log_in_as;

create function log_in_as(id bigint) returns login_result
  language plpgsql
  strict security definer
  set search_path to pg_catalog, public, pg_temp
as $$
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
$$;

revoke all on function log_in_as from anonymous;
grant all on function log_in_as to administrator;
