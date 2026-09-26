CREATE FUNCTION app_private.tg_security_event__password() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  insert into security_event (user_id, kind, method)
  values (new.id, 'password_changed', 'manual');
  return new;
end;
$$;
