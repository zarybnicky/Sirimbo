CREATE TYPE public.payment_group_item AS (
	person_id bigint,
	due_at timestamp with time zone,
	amount numeric
);
