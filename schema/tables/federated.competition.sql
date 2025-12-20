CREATE TABLE federated.competition (
    id bigint NOT NULL,
    federation text NOT NULL,
    external_id text NOT NULL,
    event_id bigint NOT NULL,
    category_id bigint NOT NULL,
    start_date date,
    end_date date,
    CONSTRAINT competition_check CHECK (((end_date IS NULL) OR (end_date >= start_date)))
);

GRANT SELECT ON TABLE federated.competition TO anonymous;

ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_federation_external_id_key UNIQUE (federation, external_id);
ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_pkey PRIMARY KEY (id);
ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_category_id_fkey FOREIGN KEY (category_id) REFERENCES federated.category(id);
ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_event_id_fkey FOREIGN KEY (event_id) REFERENCES federated.event(id);
ALTER TABLE ONLY federated.competition
    ADD CONSTRAINT competition_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);

CREATE INDEX competition_category_id_idx ON federated.competition USING btree (category_id);
CREATE INDEX competition_event_id_idx ON federated.competition USING btree (event_id);
