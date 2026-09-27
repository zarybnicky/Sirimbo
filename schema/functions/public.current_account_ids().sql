CREATE FUNCTION public.current_account_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE
    AS $$
  select id from account
  where person_id = any ((select current_person_ids())::bigint[]);
$$;

COMMENT ON FUNCTION public.current_account_ids() IS '@omit';

GRANT ALL ON FUNCTION public.current_account_ids() TO anonymous;
