CREATE TYPE public.event_overlaps_conflict AS (
	person_id bigint,
	person_name text,
	first_instance_id bigint,
	first_event_id bigint,
	first_event_name text,
	first_since timestamp with time zone,
	first_until timestamp with time zone,
	second_instance_id bigint,
	second_event_id bigint,
	second_event_name text,
	second_since timestamp with time zone,
	second_until timestamp with time zone,
	overlap_range tstzrange
);
