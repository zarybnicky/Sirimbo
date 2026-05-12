CREATE FUNCTION public.csts_athlete(idt integer) RETURNS text
    LANGUAGE sql STABLE
    AS $$
  select canonical_name from federated.person where federation = 'csts' and external_id = idt;
$$;

GRANT ALL ON FUNCTION public.csts_athlete(idt integer) TO anonymous;
