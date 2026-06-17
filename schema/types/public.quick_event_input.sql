CREATE TYPE public.quick_event_input AS (
	since timestamp with time zone,
	until timestamp with time zone,
	type public.event_type,
	location_id bigint,
	location_text text,
	trainer_person_ids bigint[],
	registrations public.quick_event_registration_input[]
);
