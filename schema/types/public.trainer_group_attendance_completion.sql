CREATE TYPE public.trainer_group_attendance_completion AS (
	person_id integer,
	total_instances integer,
	filled_instances integer,
	partially_filled_instances integer,
	unfilled_instances integer,
	filled_ratio double precision,
	total_attendances integer,
	pending_attendances integer
);
