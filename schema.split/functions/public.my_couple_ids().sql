CREATE FUNCTION public.my_couple_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE
    AS $$
  select couple.id
  from couple join person on (man_id = person.id or woman_id = person.id) join user_proxy on person_id=person.id
  where user_id = current_user_id();
$$;

COMMENT ON FUNCTION public.my_couple_ids() IS '@omit';

GRANT ALL ON FUNCTION public.my_couple_ids() TO anonymous;


