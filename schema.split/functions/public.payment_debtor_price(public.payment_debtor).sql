CREATE FUNCTION public.payment_debtor_price(p public.payment_debtor) RETURNS SETOF public.price
    LANGUAGE sql STABLE
    AS $$
  select (amount / (select count(*) from payment_debtor where p.payment_id=payment_id), account.currency)::price
  from payment_recipient join account on account_id=account.id
  where payment_id=p.payment_id;
$$;

COMMENT ON FUNCTION public.payment_debtor_price(p public.payment_debtor) IS '@simpleCollections only
@deprecated';

GRANT ALL ON FUNCTION public.payment_debtor_price(p public.payment_debtor) TO anonymous;


