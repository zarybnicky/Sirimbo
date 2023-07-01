CREATE FUNCTION public.akce_has_capacity(a public.event) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select count(*) < a.a_kapacita from akce_item where ai_id_rodic = a.a_id;
$$;

GRANT ALL ON FUNCTION public.akce_has_capacity(a public.event) TO anonymous;


