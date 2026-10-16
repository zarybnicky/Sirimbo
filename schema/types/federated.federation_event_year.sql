CREATE TYPE federated.federation_event_year AS (
	year integer,
	event_count integer,
	mapped_event_count integer
);

COMMENT ON COLUMN federated.federation_event_year.year IS '@notNull';
COMMENT ON COLUMN federated.federation_event_year.event_count IS '@notNull';
COMMENT ON COLUMN federated.federation_event_year.mapped_event_count IS '@notNull';
