CREATE FUNCTION public.wdsf_athlete(min integer) RETURNS text
    LANGUAGE sql STABLE
    AS $$
  select canonical_name
  from federated.person
  join federated.athlete on person.id = athlete.person_id
  join federated.federation_athlete on athlete.id = federation_athlete.athlete_id
  where federation = 'wdsf' and external_id = min::text;
$$;

GRANT ALL ON FUNCTION public.wdsf_athlete(min integer) TO anonymous;
