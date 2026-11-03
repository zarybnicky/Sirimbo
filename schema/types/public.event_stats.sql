CREATE TYPE public.event_stats AS (
	total integer,
	unknown integer,
	attended integer,
	not_excused integer
);

COMMENT ON COLUMN public.event_stats.total IS '@notNull';
COMMENT ON COLUMN public.event_stats.unknown IS '@notNull';
COMMENT ON COLUMN public.event_stats.attended IS '@notNull';
COMMENT ON COLUMN public.event_stats.not_excused IS '@notNull';
