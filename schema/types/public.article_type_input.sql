CREATE TYPE public.article_type_input AS (
	id bigint,
	title text,
	preview text,
	body text,
	title_photo_url text,
	is_visible boolean
);
