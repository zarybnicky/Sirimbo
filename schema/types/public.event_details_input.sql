CREATE TYPE public.event_details_input AS (
	parent_id bigint,
	name text,
	type public.event_type,
	location_id bigint,
	location_text text,
	capacity integer,
	capacity_unit public.event_capacity_unit,
	is_visible boolean,
	is_public boolean,
	has_public_details boolean,
	is_locked boolean,
	enable_notes boolean
);
