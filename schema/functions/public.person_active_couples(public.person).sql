CREATE FUNCTION public.person_active_couples(p public.person) RETURNS SETOF public.couple
    LANGUAGE sql STABLE
    AS $$
  (select * from couple where man_id = p.id and status = 'active')
  union all
  (select * from couple where woman_id = p.id and status = 'active')
$$;

COMMENT ON FUNCTION public.person_active_couples(p public.person) IS '@behavior +list -connection';

GRANT ALL ON FUNCTION public.person_active_couples(p public.person) TO anonymous;
