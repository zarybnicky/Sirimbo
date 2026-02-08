CREATE or replace FUNCTION app_private.tg_account_balances__update() RETURNS trigger
  LANGUAGE plpgsql SECURITY DEFINER
  SET search_path TO 'pg_catalog', 'public', 'pg_temp'
AS $$
BEGIN
  PERFORM graphile_worker.add_job(
    'refresh_account_balances',
    job_key := 'refresh_account_balances'
  );
  return null;
END
$$;
