CREATE FUNCTION public.person_primary_phone(p public.person) RETURNS text
    LANGUAGE sql STABLE
    AS $$
  select phone from person_phone where person_id = p.id and is_primary = true;
$$;

GRANT ALL ON FUNCTION public.person_primary_phone(p public.person) TO anonymous;


