
ALTER TABLE ONLY event_registration
    DROP CONSTRAINT event_registration_target_cohort_id_fkey,
    ADD CONSTRAINT event_registration_target_cohort_id_fkey FOREIGN KEY (target_cohort_id) REFERENCES event_target_cohort(id) ON UPDATE CASCADE ON DELETE CASCADE;

drop function if exists create_credit_transaction_for_person;
create or replace function create_credit_transaction_for_person(v_person_id bigint, v_description text, v_amount numeric(19, 4), v_currency text, v_date timestamp with time zone DEFAULT now()) returns transaction language sql as $$
  with txn as (
    insert into transaction (source, description, effective_date) values ('manual-credit', v_description, v_date) returning *
  ), posting as (
    insert into posting (transaction_id, account_id, amount) values ((select id from txn), (select id from person_account(V_person_id, v_currency)), v_amount)
  )
  select * from txn;
$$;
grant all on function create_credit_transaction_for_person to anonymous;

COMMENT ON TABLE public.payment IS '@omit create
@simpleCollections only';

drop function if exists create_credit_transaction;

CREATE or replace FUNCTION app_private.tg_cohort_membership__on_status() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if NEW.status = 'expired' and (TG_OP = 'INSERT' or OLD.status <> NEW.status) then
    -- with affected as (
    --   select cohort_membership.until, event_registration.person_id, event_registration.id as registration_id
    --   from event_target_cohort
    --   join event_registration on event_target_cohort.event_id=event_registration.event_id
    --   join cohort_membership on cohort_membership.person_id=event_registration.person_id and cohort_membership.cohort_id=event_target_cohort.cohort_id
    --   where cohort_membership.until is not null
    --     and cohort_membership.id = OLD.id
    -- )
    -- update event_attendance set status = 'cancelled'
    -- where id in (
    --   select event_attendance.id
    --   from event_attendance
    --   join event_instance on event_instance.id = event_attendance.instance_id
    --   join affected on event_attendance.registration_id = affected.registration_id
    --    and affected.until < event_instance.since
    --    and status in ('unknown', 'not-excused', 'excused')
    -- );

    -- with affected as (
    --   select cohort_membership.until, event_registration.person_id, event_registration.id as registration_id
    --   from event_target_cohort
    --   join event_registration on event_target_cohort.event_id=event_registration.event_id
    --   join cohort_membership on cohort_membership.person_id=event_registration.person_id and cohort_membership.cohort_id=event_target_cohort.cohort_id
    --   where cohort_membership.until is not null
    --   and cohort_membership.id = OLD.id
    -- )
    -- delete from event_registration
    -- where exists (
    --   select event_attendance.id
    --   from event_attendance
    --   join event_instance on event_instance.id = event_attendance.instance_id
    --   join affected on event_attendance.registration_id = affected.registration_id
    --    and affected.until < event_instance.since
    --    and status = 'cancelled'
    -- ) and not exists (
    --   select event_attendance.id
    --   from event_attendance
    --   join event_instance on event_instance.id = event_attendance.instance_id
    --   join affected on event_attendance.registration_id = affected.registration_id
    --    and affected.until < event_instance.since
    --    and status <> 'cancelled'
    -- );

  elsif NEW.status = 'active' and (TG_OP = 'INSERT' or OLD.status <> NEW.status) then
    -- add payments

    insert into event_registration (event_id, target_cohort_id, person_id, tenant_id)
    select event.id, event_target_cohort.id, cohort_membership.person_id, cohort_membership.tenant_id
    from event
    join event_target_cohort on event_id=event.id
    join cohort_membership on event_target_cohort.cohort_id=cohort_membership.cohort_id and active_range @> now()
    left join event_registration on target_cohort_id=event_target_cohort.id and event_registration.person_id=cohort_membership.person_id and event_registration.event_id=event.id
    where event_registration.id is null
      and exists (select 1 from event_instance where event_id=event.id and until > now())
      and cohort_membership.id = NEW.id
    on conflict on constraint event_registration_unique_event_person_couple_key do nothing;
  end if;
  return NEW;
end;
$$;


drop table if exists http_cache;
create table if not exists http_cache (
    url text not null primary key,
    response json not null,
    expires_at timestamptz not null
);

comment on table http_cache is '@omit';
grant all on http_cache to anonymous;

CREATE or replace FUNCTION app_private.tg_http_cache__prune_expired() RETURNS trigger LANGUAGE plpgsql as $$
begin
    delete from http_cache where expires_at < now();
    return new;
END;
$$;
drop trigger if exists _500_prune_expired on http_cache;
CREATE TRIGGER _500_prune_expired
   AFTER INSERT OR UPDATE ON http_cache
   FOR EACH STATEMENT
   EXECUTE PROCEDURE app_private.tg_http_cache__prune_expired();

drop function if exists create_cash_deposit;
drop function if exists payment_debtor_price_temp;

comment on function create_event_instance_payment is '@omit';
comment on function resolve_payment_with_credit is '@omit';

create or replace function public.event_instance_approx_price(v_instance event_instance) returns setof price language plpgsql stable as $$
declare
  num_participants int;
  duration int;
begin
  num_participants := (select count(*) from event join lateral event_registrants(event.*) on true where event.id=v_instance.event_id);
  duration = extract(epoch from (v_instance.until - v_instance.since)) / 60;

  if exists (select 1 from event_instance_trainer where instance_id = v_instance.id) then
    return query
    select
      sum((tenant_trainer.member_price_45min).amount * duration / 45 / num_participants)::numeric(19,4),
      (tenant_trainer.member_price_45min).currency
    from event_instance_trainer join tenant_trainer on event_instance_trainer.person_id=tenant_trainer.person_id
    where active and event_instance_trainer.instance_id=v_instance.id
    group by (tenant_trainer.member_price_45min).currency;
  else
    return query
    select
      sum((tenant_trainer.member_price_45min).amount * duration / 45 / num_participants)::numeric(19,4),
      (tenant_trainer.member_price_45min).currency
    from event_trainer join tenant_trainer on event_trainer.person_id=tenant_trainer.person_id
    where active and event_trainer.event_id=v_instance.event_id
    group by (tenant_trainer.member_price_45min).currency;
  end if;
end;
$$;
grant all on function event_instance_approx_price to anonymous;
COMMENT ON FUNCTION event_instance_approx_price IS '@simpleCollections only';
