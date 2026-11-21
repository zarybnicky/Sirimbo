CREATE TYPE federated.federation_event_facets AS (
	years federated.federation_event_year[]
);

COMMENT ON TYPE federated.federation_event_facets IS '@name federation_event_facets';
COMMENT ON COLUMN federated.federation_event_facets.years IS '@notNull';
