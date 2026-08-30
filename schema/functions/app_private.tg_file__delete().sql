CREATE FUNCTION app_private.tg_file__delete() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  perform graphile_worker.add_job(
    'delete_file',
    json_build_object('object_key', old.object_key)
  );
  return old;
end;
$$;
