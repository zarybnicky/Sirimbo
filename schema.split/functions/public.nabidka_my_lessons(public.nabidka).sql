CREATE FUNCTION public.nabidka_my_lessons(n public.nabidka) RETURNS integer
    LANGUAGE sql STABLE
    AS $$
  select COALESCE(ni_pocet_hod, 0) from nabidka_item where n.n_id = ni_id_rodic
  and ni_partner in (select * from current_couple_ids());
$$;

GRANT ALL ON FUNCTION public.nabidka_my_lessons(n public.nabidka) TO anonymous;


