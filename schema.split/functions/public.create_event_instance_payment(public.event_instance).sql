CREATE FUNCTION public.create_event_instance_payment(i public.event_instance) RETURNS public.payment
    LANGUAGE plpgsql
    AS $$
declare
  e event;
  payment payment;
  duration numeric(19, 4);
  counter int;
begin
  select * into payment from payment where event_instance_id = i.id;
  if found then
    return payment;
  end if;
  select * into e from event where id = i.event_id;
  if e.payment_type <> 'after_instance' or not exists (select * from event_registration where event_id=e.id) then
    return null;
  end if;

  duration := extract(epoch from (i.until - i.since)) / 60;

  insert into payment (accounting_period_id, status, event_instance_id, due_at)
  values ((select id from accounting_period where range @> now()), 'tentative', i.id, now() + '2 week'::interval)
  returning * into payment;

  insert into payment_recipient (payment_id, account_id, amount)
  select payment.id, account.id, (price).amount
  from event_instance_trainer
  join tenant_trainer on event_instance_trainer.person_id = tenant_trainer.person_id and tenant_trainer.tenant_id=current_tenant_id() and tenant_trainer.active_range @> now()
  join lateral coalesce(event_instance_trainer.lesson_price, ((tenant_trainer.member_price_45min).amount / 45 * duration, (tenant_trainer.member_price_45min).currency)::price) price on true
  join lateral person_account(tenant_trainer.person_id, (price).currency) account on true
  where event_instance_trainer.instance_id=i.id and (lesson_price is not null or member_price_45min is not null);

  get diagnostics counter = row_count;
  if counter <= 0 then
    insert into payment_recipient (payment_id, account_id, amount)
    select payment.id, account.id, (price).amount
    from event_trainer
    join tenant_trainer on event_trainer.person_id = tenant_trainer.person_id and tenant_trainer.tenant_id=current_tenant_id() and tenant_trainer.active_range @> now()
    join lateral coalesce(event_trainer.lesson_price, ((tenant_trainer.member_price_45min).amount / 45 * duration, (tenant_trainer.member_price_45min).currency)::price) price on true
    join lateral person_account(tenant_trainer.person_id, (price).currency) account on true
    where event_trainer.event_id=i.event_id and (lesson_price is not null or member_price_45min is not null);
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

GRANT ALL ON FUNCTION public.create_event_instance_payment(i public.event_instance) TO anonymous;


