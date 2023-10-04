--! Previous: sha1:6f635485797f8fceea6b65eb1ccfac51ebe1dd32
--! Hash: sha1:6c19531bd8babf55e5762eb7323d559a20cc307a

--! split: 1-cleanup.sql
do $$
begin
  if not exists (select * from pg_type where typcategory='E' and typname = 'event_payment_type') then
    create type event_payment_type as enum (
      'upfront',
      'after_instance',
      'none'
    );
  end if;
end
$$;

alter table event drop column if exists registration_price;
alter table event drop column if exists use_default_price;
alter table event add column if not exists payment_type event_payment_type not null default 'none';
alter table event add column if not exists is_paid_by_tenant boolean not null default true;
alter table event add column if not exists member_price price null default null;
alter table event add column if not exists guest_price price null default null;
alter table event_instance_trainer add column if not exists lesson_price price null default null;

alter table tenant_trainer drop column if exists default_price;
alter table tenant_trainer add column if not exists member_price_45min price null default null;
alter table tenant_trainer add column if not exists member_payout_45min price null default null;
alter table tenant_trainer add column if not exists guest_price_45min price null default null;
alter table tenant_trainer add column if not exists guest_payout_45min price null default null;
alter table tenant_trainer add column if not exists create_payout_payments boolean not null default true;


CREATE or replace FUNCTION public.register_to_event_many(registrations public.register_to_event_type[]) RETURNS SETOF public.event_registration
    LANGUAGE plpgsql STRICT security definer
    AS $$
declare
  event event;
  created_ids bigint[] := array[]::bigint[];
  registration register_to_event_type;
  created event_registration;
  demand event_lesson_demand;
begin
  foreach registration in array registrations loop
    select * into event from event where id = registration.event_id;

    if event is null then
      raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
    end if;
    if event.is_locked = true then
      raise exception 'NOT_ALLOWED' using errcode = '28000';
    end if;
    if registration.person_id not in (select my_person_ids()) and registration.couple_id not in (select my_couple_ids()) then
      raise exception 'ACCESS_DENIED' using errcode = '42501';
    end if;

    insert into event_registration (event_id, person_id, couple_id, note)
    values (registration.event_id, registration.person_id, registration.couple_id, registration.note)
    returning * into created;
    created_ids := created_ids || created.id;
    foreach demand in array registration.lessons loop
      perform set_lesson_demand(created.id, demand.trainer_id, demand.lesson_count);
    end loop;
  end loop;
  return query select * from event_registration where id = any (created_ids);
end;
$$;

alter table tenant_location alter column description type text;
alter table tenant_location alter column description set default '';
alter table tenant_location alter column description set not null;


CREATE or replace FUNCTION public.upsert_event(INOUT info public.event, instances public.event_instance[], trainers public.event_trainer[], cohorts public.event_target_cohort[], registrations public.event_registration[]) RETURNS public.event LANGUAGE plpgsql AS $$
declare
  instance event_instance;
  trainer event_trainer;
  cohort event_target_cohort;
  registration event_registration;
begin
  if info.id is not null then
    update event set name=info.name, summary=info.summary, description=info.description, type=info.type, location_id=info.location_id, location_text=info.location_text, capacity=info.capacity, is_visible=info.is_visible, is_public=info.is_public, is_locked=info.is_locked, enable_notes=info.enable_notes where id=info.id;
  else
    insert into event (name, summary, description, type, location_id, location_text, capacity, is_visible, is_public, is_locked, enable_notes)
    values (info.name, info.summary, info.description, info.type, info.location_id, info.location_text, info.capacity, info.is_visible, info.is_public, info.is_locked, info.enable_notes)
    returning * into info;
  end if;

  foreach instance in array instances loop
    if instance.id is not null then
      if instance.since is null and instance.until is null then
        delete from event_instance where id=instance.id;
      else
        update event_instance set since=instance.since, until=instance.until where id=instance.id;
      end if;
    else
      insert into event_instance (event_id, since, until) values (info.id, instance.since, instance.until);
    end if;
  end loop;

  foreach trainer in array trainers loop
    if trainer.id is not null then
      if trainer.person_id is null then
        delete from event_trainer where id=trainer.id;
      else
        update event_trainer set lessons_offered=trainer.lessons_offered where id=trainer.id;
      end if;
    else
      insert into event_trainer (event_id, person_id, lessons_offered) values (info.id, trainer.person_id, coalesce(trainer.lessons_offered, 0));
    end if;
  end loop;

  foreach cohort in array cohorts loop
    if cohort.id is not null then
      if cohort.cohort_id is null then
        delete from event_target_cohort where id=cohort.id;
      end if;
    else
      insert into event_target_cohort (event_id, cohort_id) values (info.id, cohort.cohort_id);
    end if;
  end loop;

  foreach registration in array registrations loop
    if registration.id is not null then
      if registration.person_id is null and registration.couple_id is null then
        delete from event_registration where id=registration.id;
      else
        update event_registration
        set person_id=registration.person_id, couple_id=registration.couple_id, is_confirmed=registration.is_confirmed
        where id=registration.id;
      end if;
    else
      insert into event_registration (event_id, person_id, couple_id, is_confirmed)
      values (info.id, registration.person_id, registration.couple_id, registration.is_confirmed);
    end if;
  end loop;
end;
$$;

--! split: 2-payments.sql
--
alter table person drop column if exists account_id;
alter table event drop column if exists payment_recipient_id;
drop function if exists payment_debtor_price;
drop function if exists payment_debtor_is_unpaid;
drop function if exists payment_debtor_is_tentative;
drop function if exists person_account;
drop function if exists tenant_account;
drop function if exists current_accounting_period;
drop function if exists account_balance;
drop function if exists resolve_payment;
drop function if exists resolve_payment_with_credit;
drop function if exists create_event_instance_payment;
drop function if exists create_cash_deposit;
drop function if exists create_next_cohort_subscription_payment;
drop materialized view if exists account_balances;
drop table if exists posting;
drop table if exists transaction;
drop table if exists event_payment;
drop table if exists payment_debtor;
drop table if exists payment_recipient;
drop table if exists payment;
drop table if exists cohort_subscription;
drop table if exists account;
drop table if exists posting;
drop table if exists accounting_period;
drop table if exists subscription;
drop type if exists transaction_source;

alter table event_registration drop column if exists payment_id;

alter domain price drop constraint price_check;
alter domain price add constraint price_check check (
  value is null or (
    (value).currency is not null and
    (value).amount is not null and
    length((value).currency) = 3 and
    (value).currency = upper((value).currency)
  )
);

create type transaction_source as enum (
  'auto-bank',
  'auto-credit',
  'manual-bank',
  'manual-credit',
  'manual-cash'
);

create table accounting_period (
  id bigint PRIMARY KEY GENERATED by default AS IDENTITY,
  tenant_id bigint not null references tenant (id) DEFAULT public.current_tenant_id(),
  name text not null default '',
  since timestamptz not null,
  until timestamptz not null,
  range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL,
  created_at timestamptz DEFAULT now() NOT NULL,
  updated_at timestamptz DEFAULT now() NOT NULL
);
comment on table accounting_period is E'@omit create,update,delete
@simpleCollections only';
alter table accounting_period enable row level security;
grant all on accounting_period to anonymous;
CREATE POLICY admin_manage on accounting_period to administrator using (true);
CREATE POLICY my_tenant ON accounting_period AS RESTRICTIVE USING (tenant_id = current_tenant_id());
CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON accounting_period FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

create table account (
  id bigint PRIMARY KEY GENERATED by default AS IDENTITY,
  tenant_id bigint not null references tenant (id) DEFAULT public.current_tenant_id(),
  person_id bigint null references person (id),
  name text null default null,
  opening_balance numeric(19, 4) not null default 0.0,
  currency citext not null default 'CZK',
  created_at timestamptz DEFAULT now() NOT NULL,
  updated_at timestamptz DEFAULT now() NOT NULL
);
comment on table account is E'@omit create,update,delete
@simpleCollections both';
alter table account enable row level security;
grant all on account to anonymous;
CREATE POLICY admin_manage on account to administrator using (true);
CREATE POLICY my_tenant ON account AS RESTRICTIVE USING (tenant_id = current_tenant_id());
CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON account FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE UNIQUE INDEX ON account (tenant_id, person_id, currency);

alter table event add column if not exists payment_recipient_id bigint null references account (id) default null;

-- create table payment_group (
--   id bigint PRIMARY KEY GENERATED by default AS IDENTITY,
--   tenant_id bigint not null references tenant (id) DEFAULT public.current_tenant_id(),
--   name text not null default '',
--   ?? payment model
--   ?? specific symbol
--   ?? to investigate later
--   created_at timestamptz DEFAULT now() NOT NULL,
--   updated_at timestamptz DEFAULT now() NOT NULL
-- );
-- comment on table payment_group is E'@omit';
-- alter table payment_group enable row level security;
-- grant all on payment_group to anonymous;
-- CREATE POLICY admin_manage on payment_group to administrator;
-- CREATE POLICY my_tenant ON payment_group AS RESTRICTIVE USING (tenant_id = current_tenant_id());
-- CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON payment_group FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

create table cohort_subscription (
  id bigint PRIMARY KEY GENERATED by default AS IDENTITY,
  cohort_id bigint not null references skupiny (s_id),
  tenant_id bigint not null references tenant (id) DEFAULT public.current_tenant_id(),
  account_id bigint not null references account (id),
  price price not null,
  active boolean not null default true,
  renews_on timestamptz null,
  interval interval not null default '1 month'::interval,
  created_at timestamptz DEFAULT now() NOT NULL,
  updated_at timestamptz DEFAULT now() NOT NULL
);
comment on table cohort_subscription is E'@omit create,update,delete
@simpleCollections only';
alter table cohort_subscription enable row level security;
grant all on cohort_subscription to anonymous;
CREATE POLICY admin_manage on cohort_subscription to administrator using (true);
CREATE POLICY my_tenant ON cohort_subscription AS RESTRICTIVE USING (tenant_id = current_tenant_id());
CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON cohort_subscription FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

create table payment (
  id bigint PRIMARY KEY GENERATED by default AS IDENTITY,
  tenant_id bigint not null references tenant (id) DEFAULT public.current_tenant_id(),
  accounting_period_id bigint not null references accounting_period (id),
  cohort_subscription_id bigint null references cohort_subscription (id) default null,
  event_registration_id bigint null references event_registration (id) default null,
  event_instance_id bigint null references event_instance (id) default null,
  status payment_status not null,
  variable_symbol text null,
  specific_symbol text null,
  is_auto_credit_allowed boolean not null default true,
  tags text[] not null default array[]::text[],
  due_at timestamptz NULL default null,
  paid_at timestamptz NULL default null,
  created_at timestamptz DEFAULT now() NOT NULL,
  updated_at timestamptz DEFAULT now() NOT NULL
);
comment on table payment is E'@omit create,update,delete
@simpleCollections both';
alter table payment enable row level security;
grant all on payment to anonymous;
CREATE POLICY admin_manage on payment to administrator using (true);
CREATE POLICY my_tenant ON payment AS RESTRICTIVE USING (tenant_id = current_tenant_id());
CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON payment FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

create table payment_debtor (
  id bigint PRIMARY KEY GENERATED by default AS IDENTITY,
  tenant_id bigint not null references tenant (id) DEFAULT public.current_tenant_id(),
  payment_id bigint not null references payment (id),
  person_id bigint not null references person (id)
);
comment on table payment_debtor is E'@omit create,update,delete
@simpleCollections only';
alter table payment_debtor enable row level security;
grant all on payment_debtor to anonymous;
CREATE POLICY admin_manage on payment_debtor to administrator using (true);
CREATE POLICY my_tenant ON payment_debtor AS RESTRICTIVE USING (tenant_id = current_tenant_id());

create table payment_recipient (
  id bigint PRIMARY KEY GENERATED by default AS IDENTITY,
  tenant_id bigint not null references tenant (id) DEFAULT public.current_tenant_id(),
  payment_id bigint not null references payment (id),
  account_id bigint not null references account (id),
  amount numeric(19, 4) not null
);
comment on table payment_recipient is E'@omit create,update,delete
@simpleCollections only';
alter table payment_recipient enable row level security;
grant all on payment_recipient to anonymous;
CREATE POLICY admin_manage on payment_recipient to administrator using (true);
CREATE POLICY my_tenant ON payment_recipient AS RESTRICTIVE USING (tenant_id = current_tenant_id());

create or replace function payment_debtor_price(p payment_debtor) returns setof price language sql stable as $$
  select (amount / (select count(*) from payment_debtor where p.payment_id=payment_id), account.currency)::price
  from payment_recipient join account on account_id=account.id
  where payment_id=p.payment_id;
$$;
create or replace function payment_debtor_is_unpaid(p payment_debtor) returns boolean language sql stable as $$
  select status = 'unpaid' from payment where p.payment_id=payment.id;
$$;
create or replace function payment_debtor_is_tentative(p payment_debtor) returns boolean language sql stable as $$
  select status = 'tentative' from payment where p.payment_id=payment.id;
$$;
comment on function payment_debtor_price is E'@simpleCollections only';
comment on function payment_debtor_is_unpaid is E'@filterable';
comment on function payment_debtor_is_tentative is E'@filterable';
grant all on function payment_debtor_is_unpaid to anonymous;
grant all on function payment_debtor_is_tentative to anonymous;

create table transaction (
  id bigint PRIMARY KEY GENERATED by default AS IDENTITY,
  tenant_id bigint not null references tenant (id) DEFAULT public.current_tenant_id(),
  accounting_period_id bigint not null references accounting_period (id),
  payment_id bigint null references payment (id),
  source transaction_source not null,
  created_at timestamptz DEFAULT now() NOT NULL,
  updated_at timestamptz DEFAULT now() NOT NULL
);
comment on table transaction is E'@omit create,update,delete';
alter table transaction enable row level security;
grant all on transaction to anonymous;
CREATE POLICY admin_manage on transaction to administrator using (true);
CREATE POLICY my_tenant ON transaction AS RESTRICTIVE USING (tenant_id = current_tenant_id());
CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON transaction FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

create table posting (
  id bigint PRIMARY KEY GENERATED by default AS IDENTITY,
  tenant_id bigint not null references tenant (id) DEFAULT public.current_tenant_id(),
  transaction_id bigint not null references transaction (id),
  account_id bigint not null references account (id),
  original_account_id bigint null references account (id) default null,
  amount numeric(19, 4),
  created_at timestamptz DEFAULT now() NOT NULL,
  updated_at timestamptz DEFAULT now() NOT NULL
);
comment on table posting is E'@omit create,update,delete';
alter table posting enable row level security;
grant all on posting to anonymous;
CREATE POLICY admin_manage on posting to administrator using (true);
CREATE POLICY my_tenant ON posting AS RESTRICTIVE USING (tenant_id = current_tenant_id());
CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON posting FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE MATERIALIZED VIEW account_balances as
	SELECT
		account.id as id,
		COALESCE(account.opening_balance, 0.0) + COALESCE(sum(posting.amount), 0.0) as balance
	FROM account LEFT JOIN posting ON account.id = account_id
	GROUP BY account.id;
CREATE UNIQUE INDEX ON account_balances (id);
comment on materialized view account_balances is '@omit';
grant all on account_balances to anonymous;

create or replace function account_balance(a account) returns numeric(19,4) stable language sql as $$
  select balance from account_balances where id=a.id;
$$;
grant all on function account_balance to anonymous;

CREATE or replace FUNCTION app_private.tg_payment__fill_accounting_period() RETURNS TRIGGER security definer AS $$
declare
  period accounting_period;
  since timestamptz;
begin
	if NEW.accounting_period_id is null then
    select id into NEW.accounting_period_id from accounting_period where range @> now();
    if not found then
      since := case
        when extract(month from now()) > 8
        then date_trunc('year', now()) + '8 month'::interval
        else date_trunc('year', now()) + '8 month'::interval - '1 year'::interval
      end;
      insert into accounting_period (name, since, until)
      values ('Školní rok ' || extract(year from since), since, since + '12 month'::interval - '1 day'::interval)
      returning id into NEW.accounting_period_id;
    end if;
  end if;
  return NEW;
END
$$ LANGUAGE plpgsql;

CREATE or replace FUNCTION app_private.tg_account_balances__update() RETURNS TRIGGER security definer AS $$
BEGIN
	REFRESH MATERIALIZED VIEW account_balances;
  return null;
END
$$ LANGUAGE plpgsql;

CREATE TRIGGER _200_fill_accounting_period
before INSERT ON payment
FOR EACH ROW EXECUTE PROCEDURE app_private.tg_payment__fill_accounting_period();

CREATE TRIGGER _200_fill_accounting_period
before INSERT ON transaction
FOR EACH ROW EXECUTE PROCEDURE app_private.tg_payment__fill_accounting_period();

CREATE TRIGGER _900_fix_balance_entries
AFTER INSERT OR UPDATE OF amount OR DELETE OR TRUNCATE ON posting
FOR EACH STATEMENT EXECUTE PROCEDURE app_private.tg_account_balances__update();

CREATE TRIGGER _900_fix_balance_accounts
AFTER INSERT OR UPDATE OF opening_balance OR DELETE OR TRUNCATE ON account
FOR EACH STATEMENT EXECUTE PROCEDURE app_private.tg_account_balances__update();

create or replace function person_account(p_id bigint, c text, out acc account) language sql security definer as $$
  with inserted as (
    insert into account (tenant_id, person_id, currency)
    values (current_tenant_id(), p_id, coalesce(c, 'CZK'))
    on conflict (tenant_id, person_id, currency) do nothing
    returning *
  )
  select * from inserted union select * from account where person_id=p_id and currency=c;
$$;
grant all on function person_account to anonymous;

create or replace function tenant_account(c text, out acc account) language sql security definer as $$
  with inserted as (
    insert into account (tenant_id, person_id, currency)
    values (current_tenant_id(), null, coalesce(c, 'CZK'))
    on conflict (tenant_id, person_id, currency) do nothing
    returning *
  )
  select * from inserted union select * from account where person_id is null and currency=c;
$$;
grant all on function tenant_account to anonymous;

create or replace function resolve_payment_with_credit(p payment) returns payment language plpgsql volatile as $$
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
select verify_function('resolve_payment_with_credit');
grant all on function resolve_payment_with_credit to anonymous;

create or replace function create_event_instance_payment(i event_instance) returns payment language plpgsql volatile as $$
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
  if e.payment_type <> 'after_instance' then
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

  if not e.is_paid_by_tenant then
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
grant all on function create_event_instance_payment to anonymous;

create or replace function create_next_cohort_subscription_payment(c cohort_subscription) returns setof payment language plpgsql volatile as $$
declare
  member person;
  v_specific text;
  v_payment payment;
  created_ids bigint[] := array[]::bigint[];
begin
  if not c.active then
    return;
  end if;
  update cohort_subscription set renews_on = renews_on + interval where id = c.id;
  v_specific := '' || (extract(year from c.renews_on) - 2000) || extract(month from c.renews_on) || extract(day from c.renews_on) || c.id;

  for member in select person.* from cohort_membership join person on person.id=person_id where cohort_id=c.cohort_id and c.renews_on <@ cohort_membership.active_range loop
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
      c.renews_on
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
select verify_function('create_next_cohort_subscription_payment');
grant all on function create_next_cohort_subscription_payment to anonymous;

create or replace function create_cash_deposit(p person, c price) returns posting language plpgsql volatile as $$
declare
  trans transaction;
  post posting;
begin
  insert into transaction (payment_id, accounting_period_id, source)
  values (p.id, (select id from accounting_period where range @> now()), 'manual-cash')
  returning * into trans;

  insert into posting (transaction_id, account_id, amount)
  select trans.id, r.id, (c).amount
  from person_account(p.id, (c).currency) r
  returning * into post;

  return post;
end
$$;
select verify_function('create_cash_deposit');
grant all on function create_cash_deposit to anonymous;
