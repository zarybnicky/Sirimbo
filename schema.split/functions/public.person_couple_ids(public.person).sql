CREATE FUNCTION public.person_couple_ids(p public.person) RETURNS bigint[]
    LANGUAGE sql STABLE
    AS $$
  select array_agg(couple.id)
  from couple
  where man_id = p.id or woman_id = p.id and active = true;
$$;

GRANT ALL ON FUNCTION public.person_couple_ids(p public.person) TO anonymous;


