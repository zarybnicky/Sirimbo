CREATE TYPE public.series_info AS (
	id bigint,
	name text,
	"position" integer,
	length integer,
	since timestamp with time zone,
	until timestamp with time zone
);
