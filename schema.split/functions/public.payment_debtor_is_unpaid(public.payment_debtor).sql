CREATE FUNCTION public.payment_debtor_is_unpaid(p public.payment_debtor) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select status = 'unpaid' from payment where p.payment_id=payment.id;
$$;

COMMENT ON FUNCTION public.payment_debtor_is_unpaid(p public.payment_debtor) IS '@filterable';

GRANT ALL ON FUNCTION public.payment_debtor_is_unpaid(p public.payment_debtor) TO anonymous;


