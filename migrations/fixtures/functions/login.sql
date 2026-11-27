create or replace function login(login text, passwd text) returns login_result
  language plpgsql security definer
  set search_path to pg_catalog, public, pg_temp
as $$
declare
  v_salt text;
  usr users;
  jwt jwt_token;
begin
  v_salt := encode(digest('######TK.-.OLYMP######', 'md5'), 'hex');

  select u.* into usr
  from users u
  where (lower(u.u_login) = lower(trim(login)) or lower(u.u_email) = lower(trim(login)))
    and u.u_pass = encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex');

  if usr is null then
    insert into security_event (user_id, kind, method)
    values ((select id from users
      where lower(u_login) = lower(trim(login)) or lower(u_email) = lower(trim(login))
      limit 1), 'login_failed', 'password');
    return null;
  end if;

  jwt := app_private.create_jwt_token(usr);
  perform app_private.set_login_claims(jwt);
  update users set last_login = now() where id = usr.id;
  insert into security_event (user_id, kind, method)
  values (usr.id, 'login_succeeded', 'password');
  return (usr, jwt);
end;
$$;

grant all on function login to anonymous;
