CREATE FUNCTION public.account_balance(a public.account) RETURNS numeric
    LANGUAGE sql STABLE
    AS $$
  select balance from account_balances where id=a.id;
$$;

GRANT ALL ON FUNCTION public.account_balance(a public.account) TO anonymous;


