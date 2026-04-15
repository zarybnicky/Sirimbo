CREATE TYPE federated.competitor_category_progress_input AS (
	category_id bigint,
	points numeric(10,3),
	domestic_finale integer,
	foreign_finale integer
);
