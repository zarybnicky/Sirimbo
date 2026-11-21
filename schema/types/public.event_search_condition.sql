CREATE TYPE public.event_search_condition AS (
	types public.event_type[],
	trainer_ids bigint[],
	location_ids bigint[],
	parent_id bigint,
	participant_ids bigint[],
	cohort_id bigint,
	couple_id bigint,
	only_mine boolean,
	relevant_only boolean,
	can_register boolean,
	with_competitions boolean,
	with_birthdays boolean
);
