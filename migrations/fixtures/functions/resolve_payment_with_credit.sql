create or replace function resolve_payment_with_credit(p payment) returns payment
  language plpgsql
as $$
declare
  trans transaction;
  recipient payment_recipient;
  acc account;
  trainer tenant_trainer;
  remaining_amount numeric(19, 4);
  total_amount numeric(19, 4);
  actual_payout numeric(19, 4);
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
      select tenant_trainer.* into trainer from tenant_trainer where acc.person_id=tenant_trainer.person_id and tenant_id=acc.tenant_id;

      if trainer is null or trainer.create_payout_payments then
        if trainer.member_payout_45min_amount is not null then
          actual_payout := remaining_amount * trainer.member_payout_45min_amount / trainer.member_price_45min_amount;
          insert into posting (transaction_id, original_account_id, account_id, amount)
          values (trans.id, recipient.account_id, (select id from tenant_account(acc.currency)), remaining_amount - actual_payout);
          remaining_amount := actual_payout;
        end if;

        insert into posting (transaction_id, account_id, amount)
        values (trans.id, recipient.account_id, remaining_amount);
      end if;
    end loop;

  remaining_amount := total_amount / (select coalesce(nullif(count(*), 0), 1) from payment_debtor where payment_id = p.id);
  for acc in select a.* from payment_debtor d join lateral person_account(d.person_id, 'CZK') a on true where payment_id = p.id loop
      insert into posting (transaction_id, account_id, amount) values (trans.id, acc.id, 0.0 - remaining_amount);
    end loop;

  update payment set status = 'paid', paid_at = now() where id=p.id returning * into p;
  return p;
end
$$;

COMMENT ON FUNCTION resolve_payment_with_credit IS '@behavior -*';

GRANT ALL ON FUNCTION resolve_payment_with_credit TO anonymous;

create or replace function app_private.resolve_due_payments() returns setof payment
  language plpgsql
as $$
declare
  tenant_ids bigint[] := array(select id from tenant);
  v_tenant_id bigint;
  payment_row payment;
begin
  if not row_security_active('payment') then
    set local role to administrator;
  end if;
  foreach v_tenant_id in array tenant_ids loop
    perform set_config('jwt.claims.tenant_id', v_tenant_id::text, true);
    perform set_config('jwt.claims.my_tenant_ids', json_build_array(v_tenant_id)::text, true);
    for payment_row in
      update payment p set status = 'unpaid'
      where p.tenant_id = v_tenant_id and p.cohort_subscription_id is not null
        and p.due_at <= now()
        and (p.status = 'tentative' or (p.status = 'unpaid' and p.is_auto_credit_allowed))
      returning p.*
    loop
      return next coalesce(resolve_payment_with_credit(payment_row), payment_row);
    end loop;
  end loop;
end;
$$;
