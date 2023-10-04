CREATE FUNCTION app_private.tg_account_balances__update() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
BEGIN
	REFRESH MATERIALIZED VIEW account_balances;
  return null;
END
$$;



