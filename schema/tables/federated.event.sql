CREATE TABLE federated.event (
    id bigint NOT NULL,
    federation text NOT NULL,
    external_id text NOT NULL,
    name text,
    start_date date NOT NULL,
    end_date date,
    location text,
    country text,
    organizing_club_id bigint,
    range daterange GENERATED ALWAYS AS (
CASE
    WHEN (end_date IS NULL) THEN daterange(start_date, start_date, '[]'::text)
    ELSE daterange(start_date, end_date, '[]'::text)
END) STORED,
    CONSTRAINT event_check CHECK (((end_date IS NULL) OR (end_date >= start_date)))
);

GRANT SELECT ON TABLE federated.event TO anonymous;

ALTER TABLE ONLY federated.event
    ADD CONSTRAINT event_federation_external_id_key UNIQUE (federation, external_id);
ALTER TABLE ONLY federated.event
    ADD CONSTRAINT event_pkey PRIMARY KEY (id);
ALTER TABLE ONLY federated.event
    ADD CONSTRAINT event_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);
ALTER TABLE ONLY federated.event
    ADD CONSTRAINT event_organizing_club_id_fkey FOREIGN KEY (organizing_club_id) REFERENCES federated.federation_club(id);

CREATE INDEX event_federation_start_date_idx ON federated.event USING btree (federation, start_date);
CREATE INDEX event_range_idx ON federated.event USING gist (range);
