CREATE TYPE public.register_to_event_type AS (
	event_id bigint,
	person_id bigint,
	couple_id bigint,
	note text,
	lessons public.event_lesson_demand[]
);



