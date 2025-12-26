CREATE or replace FUNCTION person_account(p_id bigint, in_currency text, OUT acc account) RETURNS account AS $$
  WITH ins AS (
    INSERT INTO account (tenant_id, person_id, currency)
      VALUES ((select current_tenant_id()), p_id, COALESCE(in_currency, 'CZK'))
      ON CONFLICT ON CONSTRAINT account_tenant_id_person_id_currency_idx DO NOTHING
      RETURNING *
  )
  SELECT * FROM ins
  UNION ALL
  SELECT account.* FROM account
  WHERE tenant_id = (select current_tenant_id()) AND person_id = p_id AND currency = COALESCE(in_currency, 'CZK')
  LIMIT 1;
$$ language sql security definer set search_path = pg_catalog, public, pg_temp;

grant all on function person_account to anonymous;
