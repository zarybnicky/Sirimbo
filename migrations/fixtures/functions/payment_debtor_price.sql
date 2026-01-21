drop function if exists public.payment_debtor_price;

CREATE or replace FUNCTION public.payment_debtor_price(p public.payment_debtor, out amount numeric(19,4), out currency text)
  LANGUAGE sql STABLE
AS $$
SELECT
  sum(payment_recipient.amount) / (
    SELECT count(*) AS count
    FROM public.payment_debtor
    WHERE p.payment_id = payment_debtor.payment_id
  )::numeric(19,4) as amount,
  min(account.currency)::text as currency
FROM payment_recipient
  JOIN account ON payment_recipient.account_id = account.id
WHERE payment_recipient.payment_id = p.payment_id;
$$;

COMMENT ON FUNCTION public.payment_debtor_price(p public.payment_debtor) IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.payment_debtor_price(p public.payment_debtor) TO anonymous;
