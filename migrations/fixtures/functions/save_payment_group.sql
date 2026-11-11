drop function if exists save_payment_group;
drop type if exists payment_group_item;

create type payment_group_item as (
  person_id bigint,
  due_at timestamptz,
  amount numeric
);

create or replace function save_payment_group(group_id bigint, payments payment_group_item[])
returns setof payment
language plpgsql
as $$
declare
  subscription cohort_subscription;
  item payment_group_item;
  payment_row payment;
  person_row person;
begin
  select * into strict subscription from cohort_subscription where id = group_id for update;

  foreach item in array payments loop
    select p.* into payment_row from payment p join payment_debtor d on d.payment_id = p.id
    where p.cohort_subscription_id = group_id and p.due_at = item.due_at and d.person_id = item.person_id
    for update of p;

    if found then
      update payment_recipient set amount = item.amount
      where payment_id = payment_row.id and amount is distinct from item.amount;
      if not found then
        return next payment_row;
        continue;
      end if;
    else
      select * into strict person_row from person where id = item.person_id;

      insert into payment (
        cohort_subscription_id,
        due_at,
        status,
        variable_symbol,
        specific_symbol,
        is_auto_credit_allowed
      ) values (
        group_id,
        item.due_at,
        case when item.due_at > now() then 'tentative'::payment_status else 'unpaid'::payment_status end,
        regexp_replace(coalesce(nullif(person_row.tax_identification_number, ''), person_row.id::text), '[^0-9]', '', 'g'),
        '' || (extract(year from item.due_at) - 2000) || extract(month from item.due_at) || extract(day from item.due_at) || group_id,
        current_tenant_id() = 2
      )
      returning * into payment_row;

      insert into payment_debtor (payment_id, person_id) values (payment_row.id, item.person_id);
      insert into payment_recipient (payment_id, account_id, amount)
      values (payment_row.id, subscription.account_id, item.amount);
    end if;

    if payment_row.is_auto_credit_allowed then
      -- Keep the payment ID while rebuilding its credit transaction and postings.
      delete from transaction where payment_id = payment_row.id and source = 'auto-credit';
      update payment set
        status = case when due_at > now() then 'tentative'::payment_status else 'unpaid'::payment_status end,
        paid_at = null
      where id = payment_row.id returning * into payment_row;
      payment_row := coalesce(resolve_payment_with_credit(payment_row), payment_row);
    end if;

    return next payment_row;
  end loop;
end;
$$;

revoke all on function save_payment_group from anonymous;
grant execute on function save_payment_group to administrator;
