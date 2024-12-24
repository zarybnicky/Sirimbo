CREATE FUNCTION public.create_credit_transaction_for_person(v_person_id bigint, v_description text, v_amount numeric, v_currency text, v_date timestamp with time zone DEFAULT now()) RETURNS public.transaction
    LANGUAGE sql
    AS $$
  with txn as (
    insert into transaction (source, description, effective_date) values ('manual-credit', v_description, v_date) returning *
  ), posting as (
    insert into posting (transaction_id, account_id, amount) values ((select id from txn), (select id from person_account(V_person_id, v_currency)), v_amount)
  )
  select * from txn;
$$;

GRANT ALL ON FUNCTION public.create_credit_transaction_for_person(v_person_id bigint, v_description text, v_amount numeric, v_currency text, v_date timestamp with time zone) TO anonymous;
