CREATE FUNCTION public.csts_athlete(idt integer) RETURNS text
    LANGUAGE sql STABLE
    AS $$
  select canonical_name
  from federated.person
  join federated.athlete on person.id = athlete.person_id
  join federated.federation_athlete on athlete.id = federation_athlete.athlete_id
  where federation = 'csts' and external_id = idt::text;
$$;

GRANT ALL ON FUNCTION public.csts_athlete(idt integer) TO anonymous;
