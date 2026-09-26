CREATE FUNCTION app_private.tg_relationship__status() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  new.status := app_private.relationship_status_next(
    now(), tstzrange(new.since, new.until, '[)'), new.status
  );
  return new;
end;
$$;
