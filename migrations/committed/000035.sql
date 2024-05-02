--! Previous: sha1:5dcd1480d6a578fe4b47edf69648a199964a96fe
--! Hash: sha1:46f806b87a7c0e5a607e8ce542426327185da5e3

--! split: 1-current.sql
create or replace function create_missing_cohort_subscription_payments(c cohort_subscription) returns setof payment language plpgsql volatile as $$
declare
  member person;
  v_specific text;
  v_payment payment;
  created_ids bigint[] := array[]::bigint[];
  v_renews_on timestamptz;
begin
  if not c.active then
    return;
  end if;
  select renews_on - interval into v_renews_on from cohort_subscription where id = c.id;
  v_specific := '' || (extract(year from v_renews_on) - 2000) || extract(month from v_renews_on) || extract(day from v_renews_on) || c.id;

  for member in select person.*
    from cohort_membership
    join person on person.id=person_id
    where cohort_id=c.cohort_id
      and v_renews_on <@ cohort_membership.active_range
      and not exists (
          select 1 from payment join payment_debtor on payment_id=payment.id
          where cohort_subscription_id = c.id and due_at = v_renews_on and person_id=person.id
      ) loop
    insert into payment (
      status,
      specific_symbol,
      variable_symbol,
      cohort_subscription_id,
      accounting_period_id,
      is_auto_credit_allowed,
      due_at
    ) values (
      'unpaid',
      v_specific,
      regexp_replace(coalesce(nullif(member.tax_identification_number, ''), member.id::text), '[^0-9]', '', 'g'),
      c.id,
      (select id from accounting_period where range @> now()),
      current_tenant_id() <> 1,
      v_renews_on
    ) returning * into v_payment;
    created_ids := created_ids || v_payment.id;

    insert into payment_recipient (payment_id, account_id, amount)
    values (v_payment.id, c.account_id, (c.price).amount);

    insert into payment_debtor (payment_id, person_id)
    values (v_payment.id, member.id);
  end loop;

  return query select * from payment where id = any (created_ids);
end
$$;
select verify_function('create_missing_cohort_subscription_payments');
grant all on function create_missing_cohort_subscription_payments to anonymous;

ALTER TABLE ONLY public.event_trainer
    DROP CONSTRAINT IF EXISTS event_trainer_trainer_id_key,
    ADD CONSTRAINT event_trainer_trainer_id_key UNIQUE (event_id, person_id);

create or replace function payment_debtor_price_temp(p payment_debtor) returns price language sql stable
begin atomic
  select (sum(amount) / (select count(*) from payment_debtor where p.payment_id=payment_id), MIN(account.currency))::price
  from payment_recipient join account on account_id=account.id
  where payment_id=p.payment_id;
end;
comment on function payment_debtor_price_temp is E'@simpleCollections only';
grant all on function payment_debtor_price_temp to anonymous;


create or replace function app_private.calculate_transaction_effective_date(t transaction) returns timestamptz language sql volatile
begin atomic
  select coalesce(
    (select since from payment join event_instance on event_instance_id=event_instance.id where t.payment_id=payment.id),
    (select since from payment join event_registration on event_registration_id=event_registration.id join event_instance on event_instance.event_id=event_registration.event_id where t.payment_id=payment.id order by since asc limit 1),
    (select due_at from payment where t.payment_id=payment.id),
    t.created_at
  );
end;

CREATE or replace FUNCTION app_private.tg_transaction__effective_date() RETURNS trigger
    LANGUAGE plpgsql security definer
    AS $$
begin
  if NEW.effective_date is null then
    NEW.effective_date = app_private.calculate_transaction_effective_date(NEW);
  end if;
  return NEW;
end;
$$;

comment on column person.middle_name is '@deprecated';
comment on function payment_debtor_price is E'@simpleCollections only
@deprecated';


create or replace function app_private.create_latest_lesson_payments() returns setof payment language plpgsql volatile as $$
declare
  v_id bigint;
  created_ids bigint[] := array[]::bigint[];
begin
  perform set_config('jwt.claims.tenant_id', '2', true);
  perform set_config('jwt.claims.my_tenant_ids', '[2]', true);
  if not (select row_security_active('event')) then
    set local role to administrator;
  end if;

  update event set payment_type='after_instance'
  where type='lesson' and payment_type <> 'after_instance';

  select array_agg((create_event_instance_payment(event_instance)).id) into created_ids
  from event_instance join event on event.id=event_id
  where type='lesson'
    and event_instance.since < now()
    and payment_type = 'after_instance'
    and not exists (
      select * from payment where event_instance_id=event_instance.id
    );

  update payment set status ='unpaid' where id = any (created_ids);

  foreach v_id in array created_ids loop
    perform resolve_payment_with_credit(payment.*) from payment where id = v_id;
  end loop;

  return query select * from payment where id = any (created_ids);
end;
$$;
select verify_function('app_private.create_latest_lesson_payments');

-- select cron.schedule('create Kometa payments', '0 4 * * *', 'select app_private.create_latest_lesson_payments()');
