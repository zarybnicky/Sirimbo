--! Previous: sha1:88451f1ab754aa74bb2c07e5ca70a6e2d06a46bc
--! Hash: sha1:77545a9aba9a9b99fcfea9eb6234b6ab0b765683

--! split: 1-current.sql
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

create or replace function app_private.register_new_cohort_member_to_events(c cohort_membership)
    returns setof event_registration
    language sql as $$
  insert into event_registration (event_id, target_cohort_id, person_id, tenant_id)
  select event.id, event_target_cohort.id, cohort_membership.person_id, cohort_membership.tenant_id
  from event
  join event_target_cohort on event_id=event.id
  join cohort_membership on event_target_cohort.cohort_id=cohort_membership.cohort_id and active_range @> now()
  left join event_registration on target_cohort_id=event_target_cohort.id and event_registration.person_id=cohort_membership.person_id and event_registration.event_id=event.id
  where event_registration.id is null
    and exists (select 1 from event_instance where event_id=event.id and until > now())
    and cohort_membership.id = c.id
    and event.tenant_id = c.tenant_id
  on conflict on constraint event_registration_unique_event_person_couple_key do nothing
  returning event_registration.*;
$$;

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
    perform app_private.register_new_cohort_member_to_events(NEW);
  end if;
  return NEW;
end;
$$;
select verify_function('app_private.tg_cohort_membership__on_status', 'cohort_membership');

drop function if exists create_cash_deposit;
drop function if exists payment_debtor_price_temp;

comment on function create_event_instance_payment is '@omit';
comment on function resolve_payment_with_credit is '@omit';

create or replace function public.event_instance_approx_price(v_instance event_instance) returns setof price language plpgsql stable as $$
declare
  num_participants bigint;
  duration numeric;
begin
  num_participants := (select count(*) from event join lateral event_registrants(event.*) on true where event.id=v_instance.event_id);
  duration = extract(epoch from (v_instance.until - v_instance.since)) / 60;

  if exists (select 1 from event_instance_trainer where instance_id = v_instance.id) then
    return query
    select (
      sum((tenant_trainer.member_price_45min).amount * duration / 45 / num_participants)::numeric(19,4),
      (tenant_trainer.member_price_45min).currency
    )::price
    from event_instance_trainer join tenant_trainer on event_instance_trainer.person_id=tenant_trainer.person_id
    where active and event_instance_trainer.instance_id=v_instance.id and tenant_trainer.tenant_id = event_instance_trainer.tenant_id
    group by (tenant_trainer.member_price_45min).currency;
  else
    return query
    select (
      sum((tenant_trainer.member_price_45min).amount * duration / 45 / num_participants)::numeric(19,4),
      (tenant_trainer.member_price_45min).currency
    )::price
    from event_trainer join tenant_trainer on event_trainer.person_id=tenant_trainer.person_id
    where active and event_trainer.event_id=v_instance.event_id and tenant_trainer.tenant_id = event_trainer.tenant_id
    group by (tenant_trainer.member_price_45min).currency;
  end if;
end;
$$;

select verify_function('event_instance_approx_price');

grant all on function event_instance_approx_price to anonymous;

COMMENT ON FUNCTION event_instance_approx_price IS '@simpleCollections only';


CREATE TABLE if not exists response_cache (
    id bigint NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    url TEXT UNIQUE NOT NULL,
    status int not null,
    content TEXT NOT NULL,
    content_type TEXT NOT NULL,
    cached_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
grant all on table response_cache to anonymous;
comment on table response_cache is '@omit';

CREATE OR REPLACE FUNCTION fetch_with_cache(input_url TEXT, headers http_header[] = null)
RETURNS response_cache LANGUAGE plpgsql AS $$
DECLARE
  new_response record;
  cached_response response_cache;
BEGIN
  SELECT * INTO cached_response FROM response_cache WHERE url = input_url;

  IF NOT FOUND THEN
    SELECT * INTO new_response FROM http(('GET', input_url, headers, NULL, NULL));

    INSERT INTO response_cache (url, status, content, content_type)
    VALUES (input_url, new_response.status, new_response.content, new_response.content_type)
    ON CONFLICT (url) DO UPDATE
    SET status = EXCLUDED.status, content = EXCLUDED.content, content_type = EXCLUDED.content_type, cached_at = NOW()
    RETURNING * INTO cached_response;
  END IF;

  RETURN cached_response;
END;
$$;
select verify_function('fetch_with_cache');
comment on function fetch_with_cache is '@omit';


drop table if exists http_cache cascade;

drop function if exists public.my_event_instances_for_range;
drop function if exists public.event_instances_for_range;
CREATE or replace FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, only_mine boolean = false, trainer_ids bigint[] = null) RETURNS SETOF event_instance LANGUAGE sql STABLE
begin atomic
  select event_instance.*
  from event_instance
  join event on event_id=event.id
  where event.is_visible
    and event_instance.since <= end_range
    and event_instance.until >= start_range
    and (only_type is null or event.type = only_type)
    and (trainer_ids is null
      or exists (select 1 from event_trainer where person_id = any (trainer_ids) and event_id = event.id)
      or exists (select 1 from event_instance_trainer where person_id = any (trainer_ids) and instance_id=event_instance.id));
end;
COMMENT ON FUNCTION public.event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.event_instances_for_range TO anonymous;

CREATE or replace FUNCTION public.my_event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, only_mine boolean = false) RETURNS SETOF event_instance LANGUAGE sql STABLE
begin atomic
  select distinct on (instances.id) instances.*
  from event_instances_for_range(only_type, start_range, end_range) instances
  left join event_registration on event_registration.event_id=instances.event_id and (event_registration.person_id = any(array(select my_person_ids())) or event_registration.couple_id = any(array(select my_couple_ids())))
  left join event_trainer on event_trainer.event_id=instances.event_id and event_trainer.person_id = any(array(select my_person_ids()))
  left join event_instance_trainer on event_instance_trainer.instance_id=instances.id and event_instance_trainer.person_id = any(array(select my_person_ids()))
  where event_registration.id is not null or event_trainer.id is not null or event_instance_trainer.id is not null;
end;
COMMENT ON FUNCTION public.my_event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.my_event_instances_for_range TO anonymous;


drop function if exists filtered_people;
 --!include functions/filtered_people.sql

-- create trigger ??? after create or update on event_instance
--   if OLD.is_cancelled <> NEW.is_cancelled
--   call function delete_instance_payment;

--! split: 2-add-cancelled.sql
do $$
begin
    if not exists (
        select 1 from pg_catalog.pg_enum as enum_value
        inner join pg_catalog.pg_type as custom_type on custom_type.oid = enum_value.enumtypid
        where typname = 'attendance_type' and enumlabel = 'cancelled'
    ) then
        alter type attendance_type add value 'cancelled';
    end if;
end$$;
