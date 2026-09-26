CREATE or replace FUNCTION change_password(new_pass text) RETURNS void
    LANGUAGE plpgsql STRICT
    AS $$
declare
  v_user_id bigint;
begin
  update users set u_pass = new_pass
  where id = current_user_id()
  returning id into v_user_id;

  if v_user_id is not null then
    perform app_private.add_security_event('password_changed', 'manual', v_user_id);
  end if;
end;
$$;

GRANT ALL ON FUNCTION change_password(new_pass text) TO anonymous;
