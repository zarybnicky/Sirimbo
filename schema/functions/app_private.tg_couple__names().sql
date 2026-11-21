CREATE FUNCTION app_private.tg_couple__names() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  select
    concat_ws(' - ', concat_ws(' ', man.first_name, man.last_name), concat_ws(' ', woman.first_name, woman.last_name)),
    concat_ws(' - ', man.last_name, woman.last_name)
  into new.name, new.short_name
  from person man, person woman
  where man.id = new.man_id and woman.id = new.woman_id
  -- A concurrent rename must finish before we read, or refresh this couple after our write.
  for share of man, woman;
  return new;
end;
$$;
