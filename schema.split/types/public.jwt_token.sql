CREATE TYPE public.jwt_token AS (
	exp integer,
	user_id bigint,
	tenant_id bigint,
	username text,
	email text,
	my_person_ids bigint[],
	my_tenant_ids bigint[],
	my_cohort_ids bigint[],
	my_couple_ids bigint[],
	is_member boolean,
	is_trainer boolean,
	is_admin boolean
);



