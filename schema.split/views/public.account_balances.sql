CREATE MATERIALIZED VIEW public.account_balances AS
 SELECT account.id,
    (COALESCE(account.opening_balance, 0.0) + COALESCE(sum(posting.amount), 0.0)) AS balance
   FROM (public.account
     LEFT JOIN public.posting ON ((account.id = posting.account_id)))
  GROUP BY account.id
  WITH NO DATA;

COMMENT ON MATERIALIZED VIEW public.account_balances IS '@omit';

GRANT ALL ON TABLE public.account_balances TO anonymous;



CREATE UNIQUE INDEX account_balances_id_idx ON public.account_balances USING btree (id);