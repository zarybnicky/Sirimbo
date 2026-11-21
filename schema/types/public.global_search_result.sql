CREATE TYPE public.global_search_result AS (
	person public.person,
	couple public.couple,
	cohort public.cohort,
	location public.location,
	event public.event,
	score real,
	"position" integer
);

COMMENT ON COLUMN public.global_search_result.score IS '@notNull';
COMMENT ON COLUMN public.global_search_result."position" IS '@notNull';
