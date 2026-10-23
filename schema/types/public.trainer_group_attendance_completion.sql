CREATE TYPE public.trainer_group_attendance_completion AS (
	person_id bigint,
	total_events integer,
	filled_events integer,
	partially_filled_events integer,
	unfilled_events integer,
	filled_ratio double precision,
	total_attendances integer,
	pending_attendances integer
);
