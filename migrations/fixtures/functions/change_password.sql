CREATE or replace FUNCTION change_password(new_pass text) RETURNS void
    LANGUAGE plpgsql STRICT
    AS $$
begin
  update users set u_pass = new_pass
  where id = current_user_id();
end;
$$;

GRANT ALL ON FUNCTION change_password(new_pass text) TO anonymous;
