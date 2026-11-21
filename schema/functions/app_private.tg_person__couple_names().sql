CREATE FUNCTION app_private.tg_person__couple_names() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  -- Re-run the couple trigger so formatting has one definition.
  update couple set name = name where man_id = new.id or woman_id = new.id;
  return new;
end;
$$;
