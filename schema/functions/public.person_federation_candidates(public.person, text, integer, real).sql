CREATE FUNCTION public.person_federation_candidates(in_person public.person, federation text, "limit" integer DEFAULT 10, threshold real DEFAULT 0.4) RETURNS SETOF public.person_federation_candidates_record
    LANGUAGE sql STABLE
    AS $_$
  select fp.external_id::integer, fp.canonical_name, fp.age_group_code,
    similarity(fp.search_name, in_person.search_name) as name_score
  from federated.person fp
  where fp.federation = $2
    and fp.external_id between 0 and 2147483647
    and fp.search_name % in_person.search_name
    and similarity(fp.search_name, in_person.search_name)
      >= greatest(0.3::real, least(coalesce(threshold, 0.4), 1::real))
  order by
    coalesce(fp.dob = in_person.birth_date, false) desc,
    name_score desc, fp.canonical_name, fp.external_id
  limit greatest(0, least(coalesce($3, 10), 50));
$_$;

COMMENT ON FUNCTION public.person_federation_candidates(in_person public.person, federation text, "limit" integer, threshold real) IS '@simpleCollections only
@notNull';

GRANT ALL ON FUNCTION public.person_federation_candidates(in_person public.person, federation text, "limit" integer, threshold real) TO anonymous;
