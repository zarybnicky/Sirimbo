CREATE TYPE public.person_federation_candidates_record AS (
	id integer,
	name text,
	age_group text,
	similarity real,
	birth_year integer,
	csts_id integer,
	wdsf_id integer,
	is_strong_match boolean
);

COMMENT ON COLUMN public.person_federation_candidates_record.id IS '@notNull';
COMMENT ON COLUMN public.person_federation_candidates_record.similarity IS '@notNull';
COMMENT ON COLUMN public.person_federation_candidates_record.is_strong_match IS '@notNull';
