CREATE TYPE public.announcement_audience_type_input AS (
	id bigint,
	cohort_id bigint,
	audience_role public.announcement_audience_role
);
