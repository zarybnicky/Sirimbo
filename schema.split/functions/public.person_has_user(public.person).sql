CREATE FUNCTION public.person_has_user(p public.person) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
  select exists (select 1 from user_proxy where person_id = p.id);
$$;

GRANT ALL ON FUNCTION public.person_has_user(p public.person) TO anonymous;


