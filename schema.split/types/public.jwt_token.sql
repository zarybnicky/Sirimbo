CREATE TYPE public.jwt_token AS (
	exp integer,
	user_id bigint,
	tenant_id bigint,
	username text,
	email text,
	my_person_ids json,
	my_tenant_ids json,
	my_cohort_ids json,
	my_couple_ids json,
	is_member boolean,
	is_trainer boolean,
	is_admin boolean
);
