CREATE FUNCTION public.create_event_payment(target_event public.event) RETURNS public.payment
    LANGUAGE plpgsql
    AS $$
declare
  created_payment payment;
  duration numeric(19, 4);
begin
  if current_tenant_id() <> 2 then
    return null;
  end if;

  -- Serialize initial charging with registration changes and their deferred sync.
  select * into target_event from event
  where id = target_event.id and tenant_id = current_tenant_id() for no key update;
  if not found or target_event.is_cancelled then return null; end if;

  select p.* into created_payment from payment p where p.event_id = target_event.id;

  if found then
    return created_payment;
  end if;

  if target_event.type <> 'lesson' or not exists (
    select 1
    from event_registration registration
    where registration.event_id = target_event.id
      and registration.person_id is not null
      and registration.registration_status = 'active'
  ) then
    return null;
  end if;

  duration := extract(epoch from (target_event.until - target_event.since)) / 60;

  insert into payment (accounting_period_id, status, event_id, due_at)
  values (
    (select id from accounting_period where tenant_id = current_tenant_id() and range @> now()),
    'tentative', target_event.id, now() + interval '2 weeks'
  )
  on conflict (event_id) where event_id is not null do nothing
  returning * into created_payment;

  if not found then
    return (select p from payment p where p.event_id = target_event.id);
  end if;

  insert into payment_recipient (payment_id, account_id, amount)
  select distinct on (account.id)
    created_payment.id,
    account.id,
    trainer.member_price_45min_amount / 45 * duration
  from event_trainers(target_event) trainer
  cross join lateral person_account(trainer.person_id, trainer.currency) account
  where trainer.tenant_id = current_tenant_id()
    and trainer.member_price_45min_amount is not null;

  insert into payment_debtor (payment_id, person_id)
  select distinct created_payment.id, registration.person_id
  from event_registration registration
  where registration.event_id = target_event.id
    and registration.person_id is not null
    and registration.registration_status = 'active';

  return created_payment;
end
$$;

COMMENT ON FUNCTION public.create_event_payment(target_event public.event) IS '@behavior -*';

GRANT ALL ON FUNCTION public.create_event_payment(target_event public.event) TO anonymous;
