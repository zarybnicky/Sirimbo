CREATE FUNCTION public.login(login text, passwd text) RETURNS public.login_result
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
declare
  v_salt text;
  usr users;
  jwt jwt_token;
begin
  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;

  select u.* into usr
  from users u
  where (lower(u.u_login) = lower(trim(login)) or lower(u.u_email) = lower(trim(login)))
    and u.u_pass = encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex');

  if usr is null then
    raise exception 'INVALID_CREDENTIALS' using errcode = '28P01';
  end if;

  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);
  update users set last_login = now() where id = usr.id;
  return (usr, jwt);
end;
$$;

GRANT ALL ON FUNCTION public.login(login text, passwd text) TO anonymous;
