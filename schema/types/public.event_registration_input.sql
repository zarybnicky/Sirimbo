CREATE TYPE public.event_registration_input AS (
	person_id bigint,
	couple_id bigint,
	target_cohort_id bigint,
	is_cancelled boolean
);
