CREATE FUNCTION public.users_date_of_oldest_payment(a public.users) RETURNS date
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  SELECT min(pi_date)
  FROM platby_item
  where pi_id_user = a.u_id
$$;

GRANT ALL ON FUNCTION public.users_date_of_oldest_payment(a public.users) TO anonymous;


