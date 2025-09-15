CREATE TYPE public.event_instance_type_input AS (
	id bigint,
	since timestamp with time zone,
	until timestamp with time zone,
	is_cancelled boolean,
	trainers public.event_instance_trainer_type_input[]
);
