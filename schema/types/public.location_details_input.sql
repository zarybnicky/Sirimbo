CREATE TYPE public.location_details_input AS (
	id bigint,
	name text,
	long_name text,
	description text,
	address public.address_domain,
	show_in_lists boolean,
	ordering integer,
	latitude double precision,
	longitude double precision
);
