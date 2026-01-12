CREATE FUNCTION public.register_without_invitation(email text, passwd text) RETURNS public.login_result
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
declare
  v_salt text;
  usr users;
  jwt jwt_token;
begin
  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  insert into users (u_email, u_pass) values (email, encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex')) returning * into usr;
  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);
  return (usr, jwt);
end
$$;

GRANT ALL ON FUNCTION public.register_without_invitation(email text, passwd text) TO anonymous;
