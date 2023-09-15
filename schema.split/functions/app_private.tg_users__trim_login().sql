CREATE FUNCTION app_private.tg_users__trim_login() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
declare
  v_salt varchar;
begin
  NEW.u_login := trim(NEW.u_login);
  return NEW;
end;
$$;



