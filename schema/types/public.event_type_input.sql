CREATE TYPE public.event_type_input AS (
	id bigint,
	name text,
	summary text,
	description text,
	type public.event_type,
	location_id bigint,
	location_text text,
	capacity integer,
	is_visible boolean,
	is_public boolean,
	is_locked boolean,
	enable_notes boolean
);
