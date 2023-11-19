-- Write your migration here

-- insert into event_registration (event_id, target_cohort_id, person_id)
-- select event.id, event_target_cohort.id, cohort_membership.person_id
-- from event
-- join event_target_cohort on event_id=event.id
-- join cohort_membership on event_target_cohort.cohort_id=cohort_membership.cohort_id and active_range @> now()
-- left join event_registration on target_cohort_id=event_target_cohort.id and event_registration.person_id=cohort_membership.person_id and event_registration.event_id=event.id
-- where cohort_membership.cohort_id in (2,11) and event_registration.id is null
-- and event.id in (select distinct event_id from event_instances_for_range(false, null, '2023-09-01') where tenant_id=1)
-- on conflict on constraint event_registration_unique_event_person_couple_key do nothing;

ALTER TABLE accounting_period
  DROP CONSTRAINT accounting_period_tenant_id_fkey,
  ADD CONSTRAINT accounting_period_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES tenant (id) on update cascade ON DELETE cascade;
ALTER TABLE account
  DROP CONSTRAINT account_tenant_id_fkey,
  ADD CONSTRAINT account_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES tenant (id) on update cascade ON DELETE cascade;
ALTER TABLE account
  DROP CONSTRAINT account_person_id_fkey,
  ADD CONSTRAINT account_person_id_fkey FOREIGN KEY (person_id) REFERENCES person (id) on update cascade ON DELETE cascade;
ALTER TABLE cohort_subscription
  DROP CONSTRAINT cohort_subscription_tenant_id_fkey,
  ADD CONSTRAINT cohort_subscription_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES tenant (id) on update cascade ON DELETE cascade;
ALTER TABLE cohort_subscription
  DROP CONSTRAINT cohort_subscription_account_id_fkey,
  ADD CONSTRAINT cohort_subscription_account_id_fkey FOREIGN KEY (account_id) REFERENCES account (id) on update cascade ON DELETE cascade;
ALTER TABLE cohort_subscription
  DROP CONSTRAINT cohort_subscription_cohort_id_fkey,
  ADD CONSTRAINT cohort_subscription_cohort_id_fkey FOREIGN KEY (cohort_id) REFERENCES skupiny (s_id) on update cascade ON DELETE cascade;
ALTER TABLE payment
  DROP CONSTRAINT payment_tenant_id_fkey,
  ADD CONSTRAINT payment_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES tenant (id) on update cascade ON DELETE cascade;
ALTER TABLE payment
  DROP CONSTRAINT payment_event_registration_id_fkey,
  ADD CONSTRAINT payment_event_registration_id_fkey FOREIGN KEY (event_registration_id) REFERENCES event_registration (id) on update cascade ON DELETE cascade;
ALTER TABLE payment
  DROP CONSTRAINT payment_event_instance_id_fkey,
  ADD CONSTRAINT payment_event_instance_id_fkey FOREIGN KEY (event_instance_id) REFERENCES event_instance (id) on update cascade ON DELETE cascade;
ALTER TABLE payment
  DROP CONSTRAINT payment_cohort_subscription_id_fkey,
  ADD CONSTRAINT payment_cohort_subscription_id_fkey FOREIGN KEY (cohort_subscription_id) REFERENCES cohort_subscription (id) on update cascade ON DELETE set null;
ALTER TABLE payment_debtor
  DROP CONSTRAINT payment_debtor_person_id_fkey,
  ADD CONSTRAINT payment_debtor_person_id_fkey FOREIGN KEY (person_id) REFERENCES person (id) on update cascade ON DELETE cascade;
ALTER TABLE payment_debtor
  DROP CONSTRAINT payment_debtor_payment_id_fkey,
  ADD CONSTRAINT payment_debtor_payment_id_fkey FOREIGN KEY (payment_id) REFERENCES payment (id) on update cascade ON DELETE cascade;
ALTER TABLE payment_debtor
  DROP CONSTRAINT payment_debtor_tenant_id_fkey,
  ADD CONSTRAINT payment_debtor_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES tenant (id) on update cascade ON DELETE cascade;
ALTER TABLE payment_recipient
  DROP CONSTRAINT payment_recipient_tenant_id_fkey,
  ADD CONSTRAINT payment_recipient_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES tenant (id) on update cascade ON DELETE cascade;
ALTER TABLE payment_recipient
  DROP CONSTRAINT payment_recipient_payment_id_fkey,
  ADD CONSTRAINT payment_recipient_payment_id_fkey FOREIGN KEY (payment_id) REFERENCES payment (id) on update cascade ON DELETE cascade;
ALTER TABLE payment_recipient
  DROP CONSTRAINT payment_recipient_account_id_fkey,
  ADD CONSTRAINT payment_recipient_account_id_fkey FOREIGN KEY (account_id) REFERENCES account (id) on update cascade ON DELETE cascade;
ALTER TABLE transaction
  DROP CONSTRAINT transaction_tenant_id_fkey, --
  ADD CONSTRAINT transaction_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES tenant (id) on update cascade ON DELETE cascade;
ALTER TABLE transaction
  DROP CONSTRAINT transaction_payment_id_fkey, --
  ADD CONSTRAINT transaction_payment_id_fkey FOREIGN KEY (payment_id) REFERENCES payment (id) on update cascade ON DELETE cascade;
ALTER TABLE posting
  DROP CONSTRAINT posting_tenant_id_fkey,
  ADD CONSTRAINT posting_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES tenant (id) on update cascade ON DELETE cascade;
ALTER TABLE posting
  DROP CONSTRAINT posting_transaction_id_fkey,
  ADD CONSTRAINT posting_transaction_id_fkey FOREIGN KEY (transaction_id) REFERENCES transaction (id) on update cascade ON DELETE cascade;


select app_private.drop_policies('public.cohort_subscription');
CREATE POLICY admin_manage ON public.cohort_subscription TO administrator USING (true);
CREATE POLICY person_view ON public.cohort_subscription for select TO anonymous USING (true);
CREATE POLICY my_tenant ON public.cohort_subscription AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));

select app_private.drop_policies('public.account');
CREATE POLICY admin_manage ON public.account TO administrator USING (true);
CREATE POLICY person_view ON public.account for select TO anonymous USING (true);
CREATE POLICY my_tenant ON public.account AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));

select app_private.drop_policies('public.accounting_period');
CREATE POLICY admin_manage ON public.accounting_period TO administrator USING (true);
CREATE POLICY person_view ON public.accounting_period for select TO anonymous USING (true);
CREATE POLICY my_tenant ON public.accounting_period AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));

select app_private.drop_policies('public.payment');
CREATE POLICY admin_manage ON public.payment TO administrator USING (true);
CREATE POLICY person_view ON public.payment for select TO anonymous USING (true);
CREATE POLICY my_tenant ON public.payment AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));

select app_private.drop_policies('public.payment_debtor');
CREATE POLICY admin_manage ON public.payment_debtor TO administrator USING (true);
CREATE POLICY person_view ON public.payment_debtor for select TO anonymous USING (true);
CREATE POLICY my_tenant ON public.payment_debtor AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));

select app_private.drop_policies('public.payment_recipient');
CREATE POLICY admin_manage ON public.payment_recipient TO administrator USING (true);
CREATE POLICY person_view ON public.payment_recipient for select TO anonymous USING (true);
CREATE POLICY my_tenant ON public.payment_recipient AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));

select app_private.drop_policies('public.transaction');
CREATE POLICY admin_manage ON public.transaction TO administrator USING (true);
CREATE POLICY person_view ON public.transaction for select TO anonymous USING (true);
CREATE POLICY my_tenant ON public.transaction AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));

select app_private.drop_policies('public.posting');
CREATE POLICY admin_manage ON public.posting TO administrator USING (true);
CREATE POLICY person_view ON public.posting for select TO anonymous USING (true);
CREATE POLICY my_tenant ON public.posting AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));

alter table tenant add column if not exists bank_account text not null default '';
update tenant set bank_account='1806875329/0800' where id=1;
update tenant set bank_account='774325001/5500' where id=2;

grant all on function payment_debtor_price to anonymous;

create or replace function current_session_id() returns text as $$
  select nullif(current_setting('jwt.claims.user_id', true), '')::integer;
$$ language sql stable;
grant execute on function current_session_id to anonymous;

create or replace function current_user_id() returns bigint as $$
  SELECT nullif(current_setting('jwt.claims.user_id', true), '')::bigint;
$$ language sql stable;
grant execute on function current_user_id to anonymous;

drop function if exists app_private.log_in_as;
create or replace function app_private.log_in_as(u users) returns table (key text, value text) language sql as $$
  select 'jwt.claims.' || kv.key, set_config('jwt.claims.' || kv.key, kv.value, false)
  from app_private.create_jwt_token(u) j join lateral jsonb_each_text(to_jsonb(j)) kv on true
  union
  select 'role', set_config('role', case when is_admin then 'administrator' when is_trainer then 'trainer' when is_member then 'member' else 'anonymous' end, false)
  from app_private.create_jwt_token(u) j
$$;

create or replace function cancel_registration(registration_id bigint) returns void language plpgsql strict as $$
#variable_conflict use_variable
declare
  event event;
  reg event_registration;
begin
  select * into reg from event_registration er where er.id = registration_id;
  select * into event from event where id = reg.event_id;

  if event is null or reg is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if event.is_locked = true then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  if reg.person_id not in (select my_person_ids()) and reg.couple_id not in (select my_couple_ids()) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  delete from event_registration where id = reg.id;
end;
$$;
select verify_function('public.cancel_registration');
GRANT ALL ON FUNCTION public.cancel_registration TO anonymous;


CREATE or replace FUNCTION public.set_lesson_demand(registration_id bigint, trainer_id bigint, lesson_count integer) RETURNS public.event_lesson_demand
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
#variable_conflict use_variable
declare
  event event;
  registration event_registration;
  current_lessons int;
  lesson_demand event_lesson_demand;
begin
  select * into registration from event_registration where id = registration_id;
  select * into event from event where id = registration.event_id;
  select sum(lesson_count)::int into current_lessons from event_lesson_demand eld where eld.registration_id = registration_id;

  if lesson_count = 0 then
    delete from event_lesson_demand eld where eld.registration_id = registration.id and eld.trainer_id = trainer_id;
    return null;
  end if;

  INSERT INTO event_lesson_demand (registration_id, trainer_id, lesson_count)
  values (registration.id, trainer_id, lesson_count)
  on conflict on constraint eld_unique_registration_trainer_key do update set lesson_count = lesson_count
  returning * into lesson_demand;

  return lesson_demand;
end;
$$;

create or replace function event_instance_attendance_summary(e event_instance) returns table (status attendance_type, count int) language sql stable as $$
  select status, count(status) as count from event_attendance where instance_id=e.id group by status;
$$;
grant all on function event_instance_attendance_summary to anonymous;
comment on function event_instance_attendance_summary is '@simpleCollections only';

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
grant all on function create_event_instance_payment to anonymous;

create or replace function resolve_payment_with_credit(p payment) returns payment language plpgsql volatile as $$
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
      if trainer.member_payout_45min is not null then
        actual_payout := remaining_amount * (trainer.member_payout_45min).amount / (trainer.member_price_45min).amount;
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
select verify_function('resolve_payment_with_credit');
grant all on function resolve_payment_with_credit to anonymous;

drop function if exists filtered_people;
CREATE or replace FUNCTION public.filtered_people(in_cohort bigint, is_trainer boolean, is_admin boolean, in_cohorts bigint[] default null) RETURNS SETOF person LANGUAGE sql STABLE AS $$
  select person.* from person
  join auth_details on person_id=person.id
  where
    current_tenant_id() = any (auth_details.allowed_tenants)
    and case when in_cohorts is null then true else in_cohorts && auth_details.cohort_memberships end
    and case when in_cohort is null then true else in_cohort = any (auth_details.cohort_memberships) end
    and case when is_trainer is null then true else is_trainer = (current_tenant_id() = any (auth_details.tenant_trainers)) end
    and case when is_admin is null then true else is_admin = (current_tenant_id() = any (auth_details.tenant_administrators)) end
  order by last_name, first_name
$$;
COMMENT ON FUNCTION public.filtered_people IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.filtered_people TO anonymous;

CREATE or replace FUNCTION public.submit_form(type text, data jsonb, url text) RETURNS void LANGUAGE plpgsql STRICT SECURITY DEFINER AS $$
declare
  v_email text;
begin
  insert into form_responses (type, data, url) values (type, data, url);

  if current_tenant_id() = 1 then
    foreach v_email in array (array['m.hyzova96@seznam.cz']) loop
      perform graphile_worker.add_job(
        'send_email',
        json_build_object(
          'template', 'notify_submitted_form.mjml',
          'options', json_build_object(
          'to', v_email,
          'subject', 'Nový vyplněný formulář z webu'
        ),
        'variables', json_build_object(
          'url', url,
          'data', data
        )
      ));
    end loop;
  end if;
end;
$$;
select verify_function('submit_form');

drop table if exists membership_application;
create table membership_application (
  id bigint PRIMARY KEY GENERATED by default AS IDENTITY,
  tenant_id bigint not null DEFAULT public.current_tenant_id() references tenant (id) on update cascade on delete cascade,
    first_name text NOT NULL,
    middle_name text,
    last_name text NOT NULL,
    gender gender_type NOT NULL,
    birth_date date,
    nationality text NOT NULL,
    tax_identification_number text,
    national_id_number text,
    csts_id text,
    wdsf_id text,
    prefix_title text,
    suffix_title text,
    bio text,
    email public.citext,
    phone text,
  created_at timestamptz DEFAULT now() NOT NULL,
  updated_at timestamptz DEFAULT now() NOT NULL
);
comment on table membership_application is E'@omit create,update,delete
@simpleCollections only';
alter table membership_application enable row level security;
grant all on membership_application to anonymous;
CREATE POLICY admin_manage on membership_application to administrator using (true);
CREATE POLICY my_tenant ON membership_application AS RESTRICTIVE USING (tenant_id = current_tenant_id());
CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON membership_application FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


CREATE or replace FUNCTION public.person_account(p_id bigint, c text, OUT acc public.account) RETURNS public.account
    LANGUAGE sql SECURITY DEFINER
    AS $$
  with inserted as (
    insert into account (tenant_id, person_id, currency)
    values (current_tenant_id(), p_id, coalesce(c, 'CZK'))
    on conflict (tenant_id, person_id, currency) do nothing
    returning *
  )
  select * from inserted union select * from account where person_id=p_id and currency=c and tenant_id=current_tenant_id();
$$;

CREATE or replace FUNCTION public.tenant_account(c text, OUT acc public.account) RETURNS public.account
    LANGUAGE sql SECURITY DEFINER
    AS $$
  with inserted as (
    insert into account (tenant_id, person_id, currency)
    values (current_tenant_id(), null, coalesce(c, 'CZK'))
    on conflict (tenant_id, person_id, currency) do nothing
    returning *
  )
  select * from inserted union select * from account where person_id is null and currency=c and tenant_id=current_tenant_id();
$$;

alter table transaction add column if not exists description text null default null;
drop function if exists create_credit_transaction;

create or replace function create_credit_transaction(v_account_id bigint, v_description text, v_amount numeric(19, 4)) returns transaction language sql as $$
  with txn as (
    insert into transaction (source, description) values ('manual-credit', v_description) returning *
  ), posting as (
    insert into posting (transaction_id, account_id, amount) values ((select id from txn), v_account_id, v_amount)
  )
  select * from txn;
$$;
grant all on function create_credit_transaction to anonymous;
