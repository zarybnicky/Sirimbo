create or replace function create_event_instance_payment(i event_instance)
  returns payment
  language plpgsql
as $$
declare
  created_payment payment;
  duration numeric(19, 4);
begin
  if current_tenant_id() <> 2 then
    return null;
  end if;

  select p.* into created_payment from payment p where p.event_instance_id = i.id;

  if found then
    return created_payment;
  end if;

  if i.type <> 'lesson' or not exists (
    select 1
    from event_instance_registration registration
    where registration.instance_id = i.id
      and registration.person_id is not null
      and registration.registration_status = 'active'
  ) then
    return null;
  end if;

  duration := extract(epoch from (i.until - i.since)) / 60;

  insert into payment (accounting_period_id, status, event_instance_id, due_at)
  values (
    (select id from accounting_period where tenant_id = current_tenant_id() and range @> now()),
    'tentative', i.id, now() + interval '2 weeks'
  )
  on conflict (event_instance_id) where event_instance_id is not null do nothing
  returning * into created_payment;

  if not found then
    return (select p from payment p where p.event_instance_id = i.id);
  end if;

  insert into payment_recipient (payment_id, account_id, amount)
  select distinct on (account.id)
    created_payment.id,
    account.id,
    trainer.member_price_45min_amount / 45 * duration
  from event_instance_trainers(i) trainer
  cross join lateral person_account(trainer.person_id, trainer.currency) account
  where trainer.tenant_id = current_tenant_id()
    and trainer.member_price_45min_amount is not null;

  insert into payment_debtor (payment_id, person_id)
  select distinct created_payment.id, registration.person_id
  from event_instance_registration registration
  where registration.instance_id = i.id
    and registration.person_id is not null
    and registration.registration_status = 'active';

  return created_payment;
end
$$;

COMMENT ON FUNCTION create_event_instance_payment IS '@omit';
GRANT ALL ON FUNCTION create_event_instance_payment TO anonymous;
