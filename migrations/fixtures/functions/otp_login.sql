drop function if exists otp_login;

create or replace function otp_login(token uuid)
  returns login_result
  language plpgsql
  strict security definer
  set search_path to pg_catalog, public, pg_temp
  as $$
declare
  v_token otp_token;
  usr users;
  jwt jwt_token;
begin
  update otp_token t set used_at = now()
  where t.access_token = token and t.used_at is null and t.expires_at > now()
    and t.user_id is not null
    and (
      t.intent = 'password_reset'
      or (t.intent = 'event_handoff' and t.tenant_id = current_tenant_id())
    )
  returning t.* into v_token;
  if not found then
    raise exception 'INVALID_CREDENTIALS' using errcode = '28P01';
  end if;
  select * into usr from users where id = v_token.user_id;

  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);

  update users set last_login = now() where id = usr.id;
  insert into security_event (user_id, kind, method)
  values (usr.id, 'login_succeeded', 'otp');
  return (usr, jwt);
end;
$$;

grant all on function otp_login to anonymous;
