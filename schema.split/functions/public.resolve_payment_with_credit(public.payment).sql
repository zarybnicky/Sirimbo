CREATE FUNCTION public.resolve_payment_with_credit(p public.payment) RETURNS public.payment
    LANGUAGE plpgsql
    AS $$
declare
  trans transaction;
  recipient payment_recipient;
  acc account;
  trainer tenant_trainer;
  remaining_amount numeric(19, 4);
  total_amount numeric(19, 4);
begin
  if p.status <> 'unpaid' or not p.is_auto_credit_allowed then
    return null;
  end if;

  insert into transaction (payment_id, accounting_period_id, source)
  values (p.id, p.accounting_period_id, 'auto-credit')
  returning * into trans;

  total_amount := 0;

  for recipient in select * from payment_recipient where payment_id=p.id loop
    remaining_amount := recipient.amount;
    total_amount := total_amount + remaining_amount;

    select account.* into acc from account where id=recipient.account_id;
    select tenant_trainer.* into trainer from tenant_trainer where acc.person_id=tenant_trainer.person_id;

    if trainer is null or trainer.create_payout_payments then
      if trainer.member_payout_45min is not null then
        insert into posting (transaction_id, original_account_id, account_id, amount)
        values (trans.id, recipient.account_id, (select id from tenant_account(acc.currency)), remaining_amount - (trainer.member_payout_45min).amount);
        remaining_amount := remaining_amount - (trainer.member_payout_45min).amount;
      end if;

      insert into posting (transaction_id, account_id, amount)
      values (trans.id, recipient.account_id, remaining_amount);
    end if;
  end loop;

  remaining_amount := total_amount / (select count(*) from payment_debtor where payment_id = p.id);
  for acc in select a.* from payment_debtor d join lateral person_account(d.person_id, 'CZK') a on true where payment_id = p.id loop
    insert into posting (transaction_id, account_id, amount) values (trans.id, acc.id, 0.0 - remaining_amount);
  end loop;

  update payment set status = 'paid', paid_at = now() where id=p.id returning * into p;
  return p;
end
$$;

GRANT ALL ON FUNCTION public.resolve_payment_with_credit(p public.payment) TO anonymous;


