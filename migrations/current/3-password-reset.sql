alter table otp_token drop column if exists intent;

drop function if exists reset_password;
CREATE or replace FUNCTION public.reset_password(email character varying) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  v_tenant tenant;
  v_user users;
  v_token otp_token;
  v_people jsonb;
  v_payload jsonb := null;
begin
  for v_user in (select * from users where u_email = email) loop
    insert into otp_token (user_id)
    values (v_user.u_id) returning * into v_token;

    select jsonb_agg(person_name(person.*)) into v_people
    from user_proxy join person on person_id=person.id
    where active and user_id = v_user.u_id;

    v_payload := coalesce(v_payload, jsonb_build_array()) || jsonb_build_object(
      'login', v_user.u_login,
      'email', v_user.u_email,
      'token', v_token.access_token,
      'people', v_people
    );
  end loop;

  select * into v_tenant from tenant where id = current_tenant_id();

  if v_payload is not null then
    perform graphile_worker.add_job('forgotten_password_generate', json_build_object(
      'origin', v_tenant.origins[1],
      'intent', '/zapomenute-heslo',
      'users', v_payload
    ));
  end if;
end;
$$;

select verify_function('reset_password');
GRANT ALL ON FUNCTION reset_password TO anonymous;

drop function if exists otp_login;
CREATE or replace FUNCTION public.otp_login(token uuid, OUT usr public.users, OUT jwt public.jwt_token) RETURNS record
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  v_token otp_token;
begin
  select * into v_token from otp_token where access_token = token and used_at is null and expires_at > now();
  if not found then
    raise exception 'INVALID_CREDENTIALS' using errcode = '28P01';
  end if;
  select * into usr from users where u_id = v_token.user_id;

  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);

  update users set last_login = now() where id = usr.id;
  update otp_token set used_at = now() where id = v_token.id;
end;
$$;

-- select verify_function('otp_login');
GRANT ALL ON FUNCTION otp_login TO anonymous;
