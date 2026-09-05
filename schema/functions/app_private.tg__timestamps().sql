CREATE FUNCTION app_private.tg__timestamps() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
declare
  ignored text[];
begin
  if tg_op = 'UPDATE' then
    -- Don't bump updated_at for no-op updates
    if new is not distinct from old then
      return new;
    end if;

    select coalesce(array_agg(attribute.attname::text order by attribute.attnum), '{}') || tg_argv
      into ignored
    from pg_catalog.pg_attribute as attribute
    where attribute.attrelid = tg_relid
      and attribute.attnum > 0
      and not attribute.attisdropped
      and attribute.attgenerated <> '';

    -- Ignore generated columns and any independent state named by the trigger
    if ignored <> '{}' and to_jsonb(new) - ignored is not distinct from to_jsonb(old) - ignored then
      return new;
    end if;
  end if;

  new.created_at = case when tg_op = 'INSERT' then now() else old.created_at end;
  new.updated_at = now();
  return new;
end;
$$;

COMMENT ON FUNCTION app_private.tg__timestamps() IS 'This trigger should be called on all tables with created_at, updated_at - it ensures that they cannot be manipulated and that updated_at will always be larger than the previous updated_at.';
