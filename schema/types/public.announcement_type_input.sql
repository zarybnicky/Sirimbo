CREATE TYPE public.announcement_type_input AS (
	id bigint,
	title text,
	body text,
	is_sticky boolean,
	scheduled_since timestamp with time zone,
	scheduled_until timestamp with time zone,
	status public.announcement_status
);
