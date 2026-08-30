CREATE FUNCTION app_private.tg_announcement__status() RETURNS trigger
    LANGUAGE plpgsql
    SET search_path TO 'pg_catalog', 'public', 'app_private'
    AS $$
begin
  new.status = app_private.announcement_status_next(
    now(), new.scheduled_since, new.scheduled_until, new.status
  );
  return new;
end;
$$;
