CREATE TYPE public.event_input AS (
	id bigint,
	since timestamp with time zone,
	until timestamp with time zone,
	is_cancelled boolean,
	registrations public.event_registration_input[]
);
