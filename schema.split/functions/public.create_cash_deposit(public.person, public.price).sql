CREATE FUNCTION public.create_cash_deposit(p public.person, c public.price) RETURNS public.posting
    LANGUAGE plpgsql
    AS $$
declare
  trans transaction;
  post posting;
begin
  insert into transaction (payment_id, accounting_period_id, source)
  values (p.id, (select id from accounting_period where range @> now()), 'manual-cash')
  returning * into trans;

  insert into posting (transaction_id, account_id, amount)
  select trans.id, r.id, (c).amount
  from person_account(p.id, (c).currency) r
  returning * into post;

  return post;
end
$$;

GRANT ALL ON FUNCTION public.create_cash_deposit(p public.person, c public.price) TO anonymous;
