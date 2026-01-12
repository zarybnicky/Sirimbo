CREATE or replace FUNCTION tenant_account(in_currency text, OUT acc account) RETURNS account
  language sql security definer
  SET search_path TO pg_catalog, public, pg_temp
  AS $$
  WITH ins AS (
    INSERT INTO account (tenant_id, person_id, currency)
      VALUES ((select current_tenant_id()), NULL::bigint, COALESCE(in_currency, 'CZK'))
      ON CONFLICT ON CONSTRAINT account_tenant_id_person_id_currency_idx DO NOTHING
      RETURNING *
  )
  SELECT * FROM ins
  UNION ALL
  SELECT account.* FROM account
    WHERE tenant_id = (select current_tenant_id()) AND person_id IS NULL AND currency = COALESCE(in_currency, 'CZK')
  LIMIT 1;
$$;

grant all on function tenant_account to anonymous;
