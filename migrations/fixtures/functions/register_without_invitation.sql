create or replace function register_without_invitation(email text, passwd text) returns login_result
    language plpgsql strict security definer
    set search_path to pg_catalog, public, pg_temp
    as $$
declare
  v_salt text;
  usr users;
  jwt jwt_token;
begin
  v_salt := encode(digest('######TK.-.OLYMP######', 'md5'), 'hex');
  insert into users (u_email, u_pass) values (email, encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex')) returning * into usr;
  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);
  insert into security_event (user_id, kind, method)
  values (usr.id, 'registration', 'manual');
  return (usr, jwt);
end
$$;

grant all on function register_without_invitation to anonymous;
