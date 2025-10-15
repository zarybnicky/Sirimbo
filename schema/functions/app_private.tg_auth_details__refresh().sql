CREATE FUNCTION app_private.tg_auth_details__refresh() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
BEGIN
  perform graphile_worker.add_job('refresh_auth_details', job_key := 'refresh_auth_details');
  return null;
END
$$;
