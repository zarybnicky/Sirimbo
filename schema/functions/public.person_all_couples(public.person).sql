CREATE FUNCTION public.person_all_couples(p public.person) RETURNS SETOF public.couple
    LANGUAGE sql STABLE
    AS $$
  (select * from couple where man_id = p.id)
  union all
  (select * from couple where woman_id = p.id)
$$;

COMMENT ON FUNCTION public.person_all_couples(p public.person) IS '@behavior +list -connection';

GRANT ALL ON FUNCTION public.person_all_couples(p public.person) TO anonymous;
