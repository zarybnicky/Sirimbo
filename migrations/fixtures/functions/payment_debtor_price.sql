drop function if exists payment_debtor_price;

create or replace function payment_debtor_price(p payment_debtor, out amount numeric(19,4), out currency text)
  language sql stable
as $$
select
  sum(payment_recipient.amount) / (
    select count(*) as count
    from payment_debtor
    where p.payment_id = payment_debtor.payment_id
  )::numeric(19,4) as amount,
  min(account.currency)::text as currency
from payment_recipient
  join account on payment_recipient.account_id = account.id
where payment_recipient.payment_id = p.payment_id;
$$;

comment on function payment_debtor_price is '@simpleCollections only';
grant all on function payment_debtor_price to anonymous;
