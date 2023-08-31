CREATE FUNCTION public.person_primary_address(p public.person) RETURNS public.person_address
    LANGUAGE sql STABLE
    AS $$
  select person_address.* from person_address where person_id = p.id and is_primary = true;
$$;



