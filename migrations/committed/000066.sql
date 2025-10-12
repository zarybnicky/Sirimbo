--! Previous: sha1:c29d6a1b66fade365ef49b36820487d7ad3d9eb1
--! Hash: sha1:360b3d75f95a2a3fd99225dd7cecc28b93c9d119

--! split: 1-current.sql
CREATE or replace FUNCTION create_event_instance_payment(i event_instance) RETURNS payment
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

  if current_tenant_id() <> 2 then
    return null;
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

select verify_function('create_event_instance_payment');

COMMENT ON FUNCTION create_event_instance_payment IS '@omit';
GRANT ALL ON FUNCTION create_event_instance_payment TO anonymous;



drop function if exists filtered_people(boolean, boolean, bigint[]);
--include functions/former_filtered_people.sql
CREATE or replace FUNCTION public.filtered_people(
  is_trainer boolean,
  is_admin boolean,
  in_cohorts bigint[] default null,
  membership_state text default 'current'
) RETURNS SETOF person LANGUAGE plpgsql STABLE AS $$
begin
  if lower(coalesce(membership_state, 'current')) = 'former' then
    return query
      select *
      from public.former_filtered_people(is_trainer, is_admin, in_cohorts);
  end if;

  return query
    select person.*
    from person
    join auth_details on auth_details.person_id = person.id
    where
      current_tenant_id() = any (auth_details.allowed_tenants)
      and case
        when in_cohorts is null then true
        else in_cohorts = auth_details.cohort_memberships
          or in_cohorts && auth_details.cohort_memberships
      end
      and case
        when is_trainer is null then true
        else is_trainer = (current_tenant_id() = any (auth_details.tenant_trainers))
      end
      and case
        when is_admin is null then true
        else is_admin = (current_tenant_id() = any (auth_details.tenant_administrators))
      end
    order by last_name, first_name;
end;
$$;
COMMENT ON FUNCTION public.filtered_people IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.filtered_people TO anonymous;
