CREATE FUNCTION public.akce_free_slots(a public.event) RETURNS integer
    LANGUAGE sql STABLE
    AS $$
  select a.a_kapacita - (select count(*) from akce_item where ai_id_rodic = a.a_id);
$$;

GRANT ALL ON FUNCTION public.akce_free_slots(a public.event) TO anonymous;


