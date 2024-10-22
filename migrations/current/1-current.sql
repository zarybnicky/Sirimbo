
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

-- with affected as (
--   select cohort_membership.until, event_registration.person_id, event_registration.id as registration_id
--   from event_target_cohort
--   join event_registration on event_target_cohort.event_id=event_registration.event_id
--   join cohort_membership on cohort_membership.person_id=event_registration.person_id and cohort_membership.cohort_id=event_target_cohort.cohort_id
--   where cohort_membership.until is not null
-- )
-- update event_attendance set status = 'cancelled'
-- where id in (
--   select event_attendance.id
--   from event_attendance
--   join event_instance on event_instance.id = event_attendance.instance_id
--   join affected on event_attendance.registration_id = affected.registration_id
--    and affected.until < event_instance.since
--    and status = 'unknown'
-- );

    with affected as (
      select cohort_membership.until, event_registration.person_id, event_registration.id as registration_id
      from event_target_cohort
      join event_registration on event_target_cohort.event_id=event_registration.event_id
      join cohort_membership on cohort_membership.person_id=event_registration.person_id and cohort_membership.cohort_id=event_target_cohort.cohort_id
      where cohort_membership.until is not null
        -- and cohort_membership.id = OLD.id
    )
    delete from event_registration
    where not exists (
      select event_attendance.id
      from event_attendance
      join event_instance on event_instance.id = event_attendance.instance_id
      join affected on event_attendance.registration_id = affected.registration_id
       and affected.until < event_instance.since
       and status <> 'cancelled'
    );

CREATE or replace FUNCTION app_private.tg_cohort_membership__on_status() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if NEW.status = 'expired' and (TG_OP = 'INSERT' or OLD.status <> NEW.status) then

    with affected as (
      select cohort_membership.until, event_registration.person_id, event_registration.id as registration_id
      from event_target_cohort
      join event_registration on event_target_cohort.event_id=event_registration.event_id
      join cohort_membership on cohort_membership.person_id=event_registration.person_id and cohort_membership.cohort_id=event_target_cohort.cohort_id
      where cohort_membership.until is not null
        and cohort_membership.id = OLD.id
    )
    update event_attendance set status = 'cancelled'
    where id in (
      select event_attendance.id
      from event_attendance
      join event_instance on event_instance.id = event_attendance.instance_id
      join affected on event_attendance.registration_id = affected.registration_id
       and affected.until < event_instance.since
       and status in ('unknown', 'not-excused', 'excused')
    );

    with affected as (
      select cohort_membership.until, event_registration.person_id, event_registration.id as registration_id
      from event_target_cohort
      join event_registration on event_target_cohort.event_id=event_registration.event_id
      join cohort_membership on cohort_membership.person_id=event_registration.person_id and cohort_membership.cohort_id=event_target_cohort.cohort_id
      where cohort_membership.until is not null
      and cohort_membership.id = OLD.id
    )
    delete from event_registration
    where not exists (
      select event_attendance.id
      from event_attendance
      join event_instance on event_instance.id = event_attendance.instance_id
      join affected on event_attendance.registration_id = affected.registration_id
       and affected.until < event_instance.since
       and status <> 'cancelled'
    );

  elsif NEW.status = 'active' and (TG_OP = 'INSERT' or OLD.status <> NEW.status) then
    -- add payments
    -- add event_registrations to cohort events
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
