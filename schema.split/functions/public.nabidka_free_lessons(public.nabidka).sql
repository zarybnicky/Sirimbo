CREATE FUNCTION public.nabidka_free_lessons(n public.nabidka) RETURNS integer
    LANGUAGE sql STABLE
    AS $$
  select n.n_pocet_hod - (select sum(ni_pocet_hod) from nabidka_item where ni_id_rodic = n.n_id);
$$;

GRANT ALL ON FUNCTION public.nabidka_free_lessons(n public.nabidka) TO anonymous;


