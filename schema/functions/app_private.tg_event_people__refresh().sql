CREATE FUNCTION app_private.tg_event_people__refresh() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  perform graphile_worker.add_job(
    'refresh_event_people',
    queue_name := 'refresh_event_people',
    job_key := 'refresh_event_people'
  );
  return null;
end;
$$;
