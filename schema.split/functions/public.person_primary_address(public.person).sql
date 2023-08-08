CREATE FUNCTION public.person_primary_address(p public.person) RETURNS public.address
    LANGUAGE sql STABLE
    AS $$
  select address.* from address join person_address on address_id=address.id where person_id = p.id and is_primary = true;
$$;

GRANT ALL ON FUNCTION public.person_primary_address(p public.person) TO anonymous;


