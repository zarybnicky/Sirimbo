
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
