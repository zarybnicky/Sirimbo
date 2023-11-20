CREATE FUNCTION public.create_credit_transaction(v_account_id bigint, v_description text, v_amount numeric) RETURNS public.transaction
    LANGUAGE sql
    AS $$
  with txn as (
    insert into transaction (source, description) values ('manual-credit', v_description) returning *
  ), posting as (
    insert into posting (transaction_id, account_id, amount) values ((select id from txn), v_account_id, v_amount)
  )
  select * from txn;
$$;

GRANT ALL ON FUNCTION public.create_credit_transaction(v_account_id bigint, v_description text, v_amount numeric) TO anonymous;


