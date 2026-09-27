create or replace function app_private.visible_payment_ids() returns setof bigint
  language sql stable security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  select payment_id from public.payment_debtor
  where tenant_id = (select current_tenant_id())
    and person_id = any ((select current_person_ids())::bigint[])
  union
  select payment_id from public.payment_recipient
  where tenant_id = (select current_tenant_id())
    and account_id = any (array(select current_account_ids()));
$$;

grant execute on function app_private.visible_payment_ids() to anonymous;
