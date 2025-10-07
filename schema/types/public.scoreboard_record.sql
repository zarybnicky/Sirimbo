CREATE TYPE public.scoreboard_record AS (
	person_id bigint,
	cohort_id bigint,
	lesson_total_score bigint,
	group_total_score bigint,
	event_total_score bigint,
	manual_total_score bigint,
	total_score bigint,
	ranking bigint
);
