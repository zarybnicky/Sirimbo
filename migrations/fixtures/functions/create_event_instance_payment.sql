CREATE or replace FUNCTION create_event_instance_payment(i event_instance) RETURNS payment
    LANGUAGE plpgsql
    AS $$
declare
  e event;
  payment payment;
  duration numeric(19, 4);
  counter int;
begin
  if current_tenant_id() <> 2 then
    return null;
  end if;

  select * into payment from payment where event_instance_id = i.id;
  if found then
    return payment;
  end if;

  select * into e from event where id = i.event_id;
  if e.type <> 'lesson' or not exists (select * from event_registration where event_id=e.id) then
    return null;
  end if;

  duration := extract(epoch from (i.until - i.since)) / 60;

  insert into payment (accounting_period_id, status, event_instance_id, due_at)
  values ((select id from accounting_period where range @> now()), 'tentative', i.id, now() + '2 week'::interval)
  returning * into payment;

  insert into payment_recipient (payment_id, account_id, amount)
  select distinct on (account.id) payment.id, account.id, tenant_trainer.member_price_45min_amount / 45 * duration
  from event_instance_trainer
  join tenant_trainer on event_instance_trainer.person_id = tenant_trainer.person_id and tenant_trainer.tenant_id=current_tenant_id() and tenant_trainer.active_range @> now()
  join lateral person_account(tenant_trainer.person_id, tenant_trainer.currency) account on true
  where event_instance_trainer.instance_id=i.id and member_price_45min_amount is not null;

  get diagnostics counter = row_count;
  if counter <= 0 then
    insert into payment_recipient (payment_id, account_id, amount)
    select distinct on (account.id) payment.id, account.id, tenant_trainer.member_price_45min_amount / 45 * duration
    from event_trainer
    join tenant_trainer on event_trainer.person_id = tenant_trainer.person_id and tenant_trainer.tenant_id=current_tenant_id() and tenant_trainer.active_range @> now()
    join lateral person_account(tenant_trainer.person_id, tenant_trainer.currency) account on true
    where event_trainer.event_id=i.event_id and member_price_45min_amount is not null;
  end if;

  if e.type = 'group' and current_tenant_id() = 2 then
  else
    insert into payment_debtor (payment_id, person_id)
    select payment.id, registrant.id
    from event
    join lateral event_registrants(event) registrant on true
    where event.id=i.event_id;
  end if;

  return payment;
end
$$;

select verify_function('create_event_instance_payment');

COMMENT ON FUNCTION create_event_instance_payment IS '@omit';
GRANT ALL ON FUNCTION create_event_instance_payment TO anonymous;
