CREATE FUNCTION public.my_person_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE
    AS $$
  select person.id
  from person join user_proxy on person_id=person.id
  where user_id = current_user_id();
$$;

COMMENT ON FUNCTION public.my_person_ids() IS '@omit';

GRANT ALL ON FUNCTION public.my_person_ids() TO anonymous;


