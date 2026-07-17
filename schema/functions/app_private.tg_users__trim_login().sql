CREATE FUNCTION app_private.tg_users__trim_login() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  new.u_login := trim(new.u_login);
  return new;
end;
$$;
