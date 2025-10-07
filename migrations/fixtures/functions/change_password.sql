CREATE or replace FUNCTION public.change_password(new_pass text) RETURNS void
    LANGUAGE sql STRICT
    AS $$
  update users set u_pass = new_pass where id = current_user_id();
$$;

GRANT ALL ON FUNCTION public.change_password(new_pass text) TO anonymous;
