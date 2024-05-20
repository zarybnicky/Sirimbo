CREATE FUNCTION public.current_person_ids() RETURNS bigint[]
    LANGUAGE sql
    AS $$
  select array_agg(person_id) from user_proxy where user_id = current_user_id();
$$;

COMMENT ON FUNCTION public.current_person_ids() IS '@omit';

GRANT ALL ON FUNCTION public.current_person_ids() TO anonymous;


