
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
