CREATE FUNCTION public.person_all_couples(p public.person) RETURNS SETOF public.couple
    LANGUAGE sql STABLE
    AS $$
  select couple.* from couple where (man_id = p.id or woman_id = p.id) order by active_range;
$$;

COMMENT ON FUNCTION public.person_all_couples(p public.person) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.person_all_couples(p public.person) TO anonymous;


