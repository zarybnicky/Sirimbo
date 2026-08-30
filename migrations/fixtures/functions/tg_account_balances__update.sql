create or replace function app_private.tg_account_balances__update() returns trigger
  language plpgsql security definer
  set search_path to pg_catalog, public, pg_temp
as $$
begin
  perform graphile_worker.add_job(
    'refresh_account_balances',
    job_key := 'refresh_account_balances'
  );
  return null;
end
$$;
