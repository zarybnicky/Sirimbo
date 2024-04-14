CREATE FUNCTION public.reset_password(email character varying) RETURNS void
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

GRANT ALL ON FUNCTION public.reset_password(email character varying) TO anonymous;


