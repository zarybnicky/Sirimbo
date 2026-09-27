CREATE TYPE public.location_details_input AS (
	id bigint,
	name text,
	description text,
	address public.address_domain,
	is_public boolean
);
