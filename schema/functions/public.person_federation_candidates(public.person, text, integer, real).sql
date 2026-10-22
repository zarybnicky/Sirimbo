CREATE FUNCTION public.person_federation_candidates(in_person public.person, federation text, "limit" integer DEFAULT 10, threshold real DEFAULT 0.4) RETURNS SETOF public.person_federation_candidates_record
    LANGUAGE sql STABLE
    AS $_$
  select * from federation_person_candidates(
    in_person.first_name, in_person.last_name, $2, in_person.gender, in_person.birth_date, $3, $4
  );
$_$;

COMMENT ON FUNCTION public.person_federation_candidates(in_person public.person, federation text, "limit" integer, threshold real) IS '@simpleCollections only
@notNull';

GRANT ALL ON FUNCTION public.person_federation_candidates(in_person public.person, federation text, "limit" integer, threshold real) TO anonymous;
