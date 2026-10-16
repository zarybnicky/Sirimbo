CREATE TYPE public.person_federation_candidates_record AS (
	id integer,
	name text,
	age_group text,
	similarity real
);

COMMENT ON COLUMN public.person_federation_candidates_record.id IS '@notNull';
COMMENT ON COLUMN public.person_federation_candidates_record.similarity IS '@notNull';
