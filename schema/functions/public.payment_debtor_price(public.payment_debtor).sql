CREATE FUNCTION public.payment_debtor_price(p public.payment_debtor, OUT amount numeric, OUT currency text) RETURNS record
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
select
  sum(payment_recipient.amount) / (
    select count(*) as count
    from payment_debtor
    where p.payment_id = payment_debtor.payment_id
  )::numeric(19,4) as amount,
  min(account.currency)::text as currency
from payment_recipient
  join account on payment_recipient.account_id = account.id
where payment_recipient.payment_id = p.payment_id
  -- Calculate the complete bill only for an authorized stored debtor.
  and exists (
    select from payment_debtor d
    where d.id = p.id and d.payment_id = p.payment_id
      and d.tenant_id = (select current_tenant_id())
      and (
        (select pg_has_role(coalesce(nullif(current_setting('role'), 'none'), session_user), 'administrator', 'member'))
        or (
          (select pg_has_role(coalesce(nullif(current_setting('role'), 'none'), session_user), 'member', 'member'))
          and d.person_id = any ((select current_person_ids())::bigint[])
        )
      )
  );
$$;

COMMENT ON FUNCTION public.payment_debtor_price(p public.payment_debtor, OUT amount numeric, OUT currency text) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.payment_debtor_price(p public.payment_debtor, OUT amount numeric, OUT currency text) TO anonymous;
