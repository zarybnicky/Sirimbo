CREATE FUNCTION public.create_credit_transaction(v_account_id bigint, v_description text, v_amount numeric, v_date timestamp with time zone DEFAULT now()) RETURNS public.transaction
    LANGUAGE sql
    AS $$
  with txn as (
    insert into transaction (source, description, effective_date) values ('manual-credit', v_description, v_date) returning *
  ), posting as (
    insert into posting (transaction_id, account_id, amount) values ((select id from txn), v_account_id, v_amount) returning *
  )
  select * from txn
$$;

GRANT ALL ON FUNCTION public.create_credit_transaction(v_account_id bigint, v_description text, v_amount numeric, v_date timestamp with time zone) TO anonymous;
