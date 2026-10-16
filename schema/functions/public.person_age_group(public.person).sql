CREATE FUNCTION public.person_age_group(p public.person) RETURNS text
    LANGUAGE sql STABLE
    AS $$
  select code from federated.age_group
  where person_age @> (extract(year from current_date)::int - extract(year from p.birth_date)::int);
$$;

COMMENT ON FUNCTION public.person_age_group(p public.person) IS '@behavior -queryField';

GRANT ALL ON FUNCTION public.person_age_group(p public.person) TO anonymous;
