CREATE TABLE federated.competitor_component (
    competitor_id bigint NOT NULL,
    athlete_id bigint NOT NULL,
    role federated.competitor_role NOT NULL
);

GRANT SELECT ON TABLE federated.competitor_component TO anonymous;

ALTER TABLE ONLY federated.competitor_component
    ADD CONSTRAINT competitor_component_pkey PRIMARY KEY (competitor_id, athlete_id);
ALTER TABLE ONLY federated.competitor_component
    ADD CONSTRAINT competitor_component_athlete_id_fkey FOREIGN KEY (athlete_id) REFERENCES federated.athlete(id);
ALTER TABLE ONLY federated.competitor_component
    ADD CONSTRAINT competitor_component_competitor_id_fkey FOREIGN KEY (competitor_id) REFERENCES federated.competitor(id);

CREATE INDEX competitor_component_athlete_id_idx ON federated.competitor_component USING btree (athlete_id);
