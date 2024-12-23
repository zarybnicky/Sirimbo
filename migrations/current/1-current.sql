
ALTER TABLE ONLY event_registration
    DROP CONSTRAINT event_registration_target_cohort_id_fkey,
    ADD CONSTRAINT event_registration_target_cohort_id_fkey FOREIGN KEY (target_cohort_id) REFERENCES event_target_cohort(id) ON UPDATE CASCADE ON DELETE CASCADE;

drop function if exists create_credit_transaction_for_person;
--!include functions/create_credit_transaction_for_person.sql

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

drop function if exists create_cash_deposit;
drop function if exists payment_debtor_price_temp;

comment on function create_event_instance_payment is '@omit';
comment on function resolve_payment_with_credit is '@omit';

--!include functions/event_instance_approx_price.sql

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

--!include functions/fetch_with_cache.sql

drop table if exists http_cache cascade;

drop function if exists public.my_event_instances_for_range;
drop function if exists public.event_instances_for_range;

--!include functions/event_instances_for_range.sql
--!include functions/my_event_instances_for_range.sql

drop function if exists filtered_people;
 --!include functions/filtered_people.sql

-- create trigger ??? after create or update on event_instance
--   if OLD.is_cancelled <> NEW.is_cancelled
--   call function delete_instance_payment;
