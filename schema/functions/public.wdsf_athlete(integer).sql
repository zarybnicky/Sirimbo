CREATE FUNCTION public.wdsf_athlete(min integer) RETURNS text
    LANGUAGE sql STABLE
    AS $$
  select canonical_name from federated.person where federation = 'wdsf' and external_id = min;
$$;

GRANT ALL ON FUNCTION public.wdsf_athlete(min integer) TO anonymous;
