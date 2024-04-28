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
