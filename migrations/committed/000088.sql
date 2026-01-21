--! Previous: sha1:f6440a79c8d2a65c058f1c228b83fc9280fadab2
--! Hash: sha1:1456130de8024729f572421efd6d1dc0e92300ba

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

CREATE or replace FUNCTION public.resolve_payment_with_credit(p public.payment) RETURNS public.payment
  LANGUAGE plpgsql
AS $$
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
        if trainer.member_payout_45min_amount is not null then
          actual_payout := remaining_amount * trainer.member_payout_45min_amount / trainer.member_price_45min_amount;
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

COMMENT ON FUNCTION public.resolve_payment_with_credit(p public.payment) IS '@omit';

GRANT ALL ON FUNCTION public.resolve_payment_with_credit(p public.payment) TO anonymous;

create or replace function public.event_instance_approx_price(v_instance event_instance)
  returns table (amount numeric(19,4), currency text)
  language sql stable
as $$
  with stats as (
    select
      (select count(*)
       from event e
       join lateral event_registrants(e.*) r on true
       where e.id = v_instance.event_id)::bigint as num_participants,
      extract(epoch from (v_instance.until - v_instance.since)) / 60.0 as duration
  )
  select
    sum(tt.member_price_45min_amount * s.duration / 45 / s.num_participants) as amount,
    tt.currency as currency
  from stats s
  join lateral public.event_instance_trainers(v_instance) tt on true
  where
    s.num_participants > 0
    and s.duration > 0
    and tt.member_price_45min_amount is not null
    and tt.currency is not null
  group by tt.currency;
$$;

grant all on function event_instance_approx_price to anonymous;
COMMENT ON FUNCTION event_instance_approx_price IS '@simpleCollections only';

CREATE or replace FUNCTION app_private.create_latest_lesson_payments() RETURNS SETOF payment
  LANGUAGE plpgsql
AS $$
begin
  perform set_config('jwt.claims.tenant_id', '2', true);
  perform set_config('jwt.claims.my_tenant_ids', '[2]', true);
  if not (select row_security_active('event')) then
    set local role to administrator;
  end if;

  return query WITH created AS (
    SELECT p.*
    FROM event_instance ei
      JOIN event e ON e.id = ei.event_id
      JOIN LATERAL create_event_instance_payment(ei) p ON true
    WHERE e.type = 'lesson'
      AND NOT ei.is_cancelled
      AND ei.since < now()
      AND NOT EXISTS (
        SELECT 1
        FROM payment p
        WHERE p.event_instance_id = ei.id
      )
      AND p IS NOT NULL
  ),
  unpaid AS (
    UPDATE payment p
      SET status = 'unpaid'
      FROM created
      WHERE p.id = created.id
      RETURNING p.*
  )
  SELECT p.*
  FROM unpaid
    CROSS JOIN LATERAL resolve_payment_with_credit(unpaid.*) p
  WHERE p IS NOT NULL;
end;
$$;

select verify_function('app_private.create_latest_lesson_payments');

CREATE or replace FUNCTION app_private.tg_event_instance__delete_payment_on_cancellation() RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER AS $$
declare
  payment_id bigint;
begin
  delete from payment where event_instance_id = OLD.id;

  if not new.is_cancelled then
    select (create_event_instance_payment(event_instance)).id into payment_id
    from event_instance join event on event.id=event_id
    where type='lesson'
      and event_instance.id = NEW.id
      and not event_instance.is_cancelled
      and event_instance.since < now()
      and not exists (
        select * from payment where event_instance_id=event_instance.id
      );

    update payment set status ='unpaid' where id = payment_id;
    perform resolve_payment_with_credit(payment.*) from payment where id = payment_id;
  end if;

  return OLD;
end;
$$;

select verify_function('app_private.tg_event_instance__delete_payment_on_cancellation', 'event_instance');

DROP TRIGGER IF EXISTS _500_delete_on_cancellation on public.event_instance;

CREATE TRIGGER _500_delete_on_cancellation
  AFTER UPDATE ON public.event_instance
  FOR EACH ROW
  EXECUTE FUNCTION app_private.tg_event_instance__delete_payment_on_cancellation();

drop function if exists upsert_event;

drop type if exists event_type_input;
drop type if exists event_instance_type_input;
drop type if exists event_instance_trainer_type_input;
drop type if exists event_trainer_type_input;
drop type if exists event_target_cohort_type_input;
drop type if exists event_registration_type_input;

CREATE TYPE public.event_type_input AS (
  id bigint,
  name text,
  summary text,
  description text,
  type public.event_type,
  location_id bigint,
  location_text text,
  capacity integer,
  is_visible boolean,
  is_public boolean,
  is_locked boolean,
  enable_notes boolean
);

CREATE TYPE public.event_instance_trainer_type_input AS (
  id bigint,
  person_id bigint
);

CREATE TYPE public.event_instance_type_input AS (
  id bigint,
  since timestamp with time zone,
  until timestamp with time zone,
  is_cancelled boolean,
  trainers event_instance_trainer_type_input[]
);

CREATE TYPE public.event_trainer_type_input AS (
  id bigint,
  person_id bigint,
  lessons_offered integer
);

CREATE TYPE public.event_target_cohort_type_input AS (
  id bigint,
  cohort_id bigint
);

CREATE TYPE public.event_registration_type_input AS (
  id bigint,
  person_id bigint,
  couple_id bigint
);

CREATE OR REPLACE FUNCTION upsert_event(
  info event_type_input,
  instances event_instance_type_input[],
  trainers event_trainer_type_input[],
  cohorts event_target_cohort_type_input[],
  registrations event_registration_type_input[]
) RETURNS event LANGUAGE plpgsql AS $$
declare
  instance event_instance_type_input;
  trainer event_trainer_type_input;
  instance_trainer event_instance_trainer_type_input;
  cohort event_target_cohort_type_input;
  registration event_registration_type_input;
  v_event event;
  v_instance_id bigint;
begin
  if info.id is null then

    insert into event (
      name,
      summary,
      description,
      type,
      location_id,
      location_text,
      capacity,
      is_visible,
      is_public,
      is_locked,
      enable_notes
    )
    values (
      info.name,
      info.summary,
      info.description,
      info.type,
      info.location_id,
      info.location_text,
      info.capacity,
      info.is_visible,
      info.is_public,
      info.is_locked,
      info.enable_notes
    )
    returning * into v_event;
  else
    update event set
      name=info.name,
      summary=info.summary,
      description=info.description,
      type=info.type,
      location_id=info.location_id,
      location_text=info.location_text,
      capacity=info.capacity,
      is_visible=info.is_visible,
      is_public=info.is_public,
      is_locked=info.is_locked,
      enable_notes=info.enable_notes
    where id=info.id
    returning * into v_event;

    if not found then
      raise exception 'event % not found', info.id;
    end if;
  end if;

  foreach instance in array coalesce(instances, '{}'::event_instance_type_input[]) loop
    if instance.id is null then
      insert into event_instance (event_id, since, until, is_cancelled)
      values (v_event.id, instance.since, instance.until, instance.is_cancelled)
      returning id into v_instance_id;
    elsif instance.since is null and instance.until is null then
      delete from event_instance where id=instance.id;
      v_instance_id := null;
    else
      update event_instance
      set since=instance.since, until=instance.until, is_cancelled=instance.is_cancelled
      where id=instance.id
      returning id into v_instance_id;
    end if;

    if v_instance_id is not null then
      foreach instance_trainer in array coalesce(instance.trainers, '{}'::event_instance_trainer_type_input[]) loop
        if instance_trainer.id is null then
          insert into event_instance_trainer (instance_id, person_id)
          values (v_instance_id, instance_trainer.person_id)
          on conflict (instance_id, person_id) do nothing;
        elsif instance_trainer.person_id is null then
          delete from event_instance_trainer where id=instance_trainer.id;
        end if;
      end loop;
    end if;
  end loop;

  foreach trainer in array coalesce(trainers, '{}'::event_trainer_type_input[]) loop
    if trainer.id is null then
      insert into event_trainer (event_id, person_id, lessons_offered)
      values (v_event.id, trainer.person_id, coalesce(trainer.lessons_offered, 0))
      on conflict (event_id, person_id) do update
        set lessons_offered = coalesce(trainer.lessons_offered, 0);
    elsif trainer.person_id is null then
      delete from event_trainer where id=trainer.id;
    else
      update event_trainer set lessons_offered=trainer.lessons_offered where id=trainer.id;
    end if;
  end loop;

  foreach cohort in array coalesce(cohorts, '{}'::event_target_cohort_type_input[]) loop
    if cohort.id is null then
      insert into event_target_cohort (event_id, cohort_id)
      values (v_event.id, cohort.cohort_id)
      on conflict (event_id, cohort_id) do nothing;
    elsif cohort.cohort_id is null then
      delete from event_target_cohort where id=cohort.id;
    end if;
  end loop;

  foreach registration in array coalesce(registrations, '{}'::event_registration_type_input[]) loop
    if registration.id is null then
      insert into event_registration (event_id, person_id, couple_id)
      values (v_event.id, registration.person_id, registration.couple_id)
      on conflict (event_id, person_id, couple_id) do nothing;
    elsif registration.person_id is null and registration.couple_id is null then
      delete from event_registration where id=registration.id;
    else
      update event_registration
      set person_id=registration.person_id, couple_id=registration.couple_id
      where id=registration.id;
    end if;
  end loop;

  return v_event;
end;
$$;

select verify_function('public.upsert_event');

COMMENT ON FUNCTION public.upsert_event is null;
GRANT ALL ON FUNCTION public.upsert_event TO anonymous;

drop function if exists public.payment_debtor_price;

CREATE or replace FUNCTION public.payment_debtor_price(p public.payment_debtor, out amount numeric(19,4), out currency text)
  LANGUAGE sql STABLE
AS $$
SELECT
  sum(payment_recipient.amount) / (
    SELECT count(*) AS count
    FROM public.payment_debtor
    WHERE p.payment_id = payment_debtor.payment_id
  )::numeric(19,4) as amount,
  min(account.currency)::text as currency
FROM payment_recipient
  JOIN account ON payment_recipient.account_id = account.id
WHERE payment_recipient.payment_id = p.payment_id;
$$;

COMMENT ON FUNCTION public.payment_debtor_price(p public.payment_debtor) IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.payment_debtor_price(p public.payment_debtor) TO anonymous;


alter table event
  drop column if exists payment_type,
  drop column if exists payment_recipient_id,
  drop column if exists is_paid_by_tenant;

drop type if exists event_payment_type;

drop function if exists my_event_instances_for_range;

COMMENT ON TABLE public.attachment IS '@omit update,delete';
