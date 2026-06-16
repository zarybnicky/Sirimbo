CREATE TABLE federated.event (
    id bigint NOT NULL,
    federation text NOT NULL,
    external_id text NOT NULL,
    name text,
    start_date date NOT NULL,
    end_date date,
    location text,
    city text,
    country text,
    street_address text,
    postal_code text,
    address_note text,
    geo_reference text,
    floor_size text,
    contact_name text,
    contact_phone text,
    contact_email text,
    website_url text,
    organizing_club_id bigint,
    range daterange GENERATED ALWAYS AS (daterange(start_date, (COALESCE(end_date, start_date) + 1), '[)'::text)) STORED,
    venue_lat double precision,
    venue_lng double precision,
    venue_location_source text,
    venue_location_ref text,
    CONSTRAINT event_check CHECK (((end_date IS NULL) OR (end_date >= start_date)))
);

GRANT SELECT ON TABLE federated.event TO anonymous;

ALTER TABLE ONLY federated.event
    ADD CONSTRAINT event_federation_external_id_key UNIQUE (federation, external_id);
ALTER TABLE ONLY federated.event
    ADD CONSTRAINT event_federation_id_key UNIQUE (federation, id);
ALTER TABLE ONLY federated.event
    ADD CONSTRAINT event_pkey PRIMARY KEY (id);
ALTER TABLE ONLY federated.event
    ADD CONSTRAINT event_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);
ALTER TABLE ONLY federated.event
    ADD CONSTRAINT event_federation_organizing_club_id_fkey FOREIGN KEY (federation, organizing_club_id) REFERENCES federated.federation_club(federation, id);
ALTER TABLE ONLY federated.event
    ADD CONSTRAINT event_organizing_club_id_fkey FOREIGN KEY (organizing_club_id) REFERENCES federated.federation_club(id);

CREATE INDEX event_federation_location_country_range_idx ON federated.event USING gist (federation, location, country, range);
CREATE INDEX event_federation_start_date_idx ON federated.event USING btree (federation, start_date);
CREATE INDEX event_range_idx ON federated.event USING gist (range);
