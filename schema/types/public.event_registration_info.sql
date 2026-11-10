CREATE TYPE public.event_registration_info AS (
	registrations integer,
	people integer,
	remaining_capacity integer,
	my boolean,
	can_register boolean,
	registration_open boolean
);
