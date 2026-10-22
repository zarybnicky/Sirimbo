CREATE TYPE public.event_registration_person_input AS (
	first_name text,
	last_name text,
	gender public.gender_type,
	nationality text,
	email text,
	phone text,
	birth_date date,
	csts_id integer,
	wdsf_id integer
);
