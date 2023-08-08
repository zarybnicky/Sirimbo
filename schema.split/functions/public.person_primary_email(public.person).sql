CREATE FUNCTION public.person_primary_email(p public.person) RETURNS text
    LANGUAGE sql STABLE
    AS $$
  select email from person_email where person_id = p.id and is_primary = true;
$$;

GRANT ALL ON FUNCTION public.person_primary_email(p public.person) TO anonymous;


