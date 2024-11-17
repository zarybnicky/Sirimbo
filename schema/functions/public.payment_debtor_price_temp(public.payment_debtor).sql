CREATE FUNCTION public.payment_debtor_price_temp(p public.payment_debtor) RETURNS public.price
    LANGUAGE sql STABLE
    BEGIN ATOMIC
 SELECT (ROW(((sum(payment_recipient.amount) / (( SELECT count(*) AS count
            FROM public.payment_debtor
           WHERE ((payment_debtor_price_temp.p).payment_id = payment_debtor.payment_id)))::numeric))::numeric(19,4), (public.min(account.currency))::text))::public.price AS "row"
    FROM (public.payment_recipient
      JOIN public.account ON ((payment_recipient.account_id = account.id)))
   WHERE (payment_recipient.payment_id = (payment_debtor_price_temp.p).payment_id);
END;

COMMENT ON FUNCTION public.payment_debtor_price_temp(p public.payment_debtor) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.payment_debtor_price_temp(p public.payment_debtor) TO anonymous;
