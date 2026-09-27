CREATE FUNCTION app_private.visible_payment_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select payment_id from public.payment_debtor
  where tenant_id = (select current_tenant_id())
    and person_id = any ((select current_person_ids())::bigint[])
  union
  select payment_id from public.payment_recipient
  where tenant_id = (select current_tenant_id())
    and account_id = any (array(select current_account_ids()));
$$;

GRANT ALL ON FUNCTION app_private.visible_payment_ids() TO anonymous;
