CREATE FUNCTION public.person_account(p_id bigint, in_currency text, OUT acc public.account) RETURNS public.account
    LANGUAGE sql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
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
$$;

COMMENT ON FUNCTION public.person_account(p_id bigint, in_currency text, OUT acc public.account) IS '@omit';

GRANT ALL ON FUNCTION public.person_account(p_id bigint, in_currency text, OUT acc public.account) TO anonymous;
