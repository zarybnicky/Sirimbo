CREATE TYPE federated.federation_event_facets AS (
	years federated.federation_event_year[]
);

COMMENT ON COLUMN federated.federation_event_facets.years IS '@notNull';
