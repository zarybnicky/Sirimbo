CREATE TABLE federated.competitor_component (
    competitor_id text NOT NULL,
    person_id text NOT NULL,
    role federated.competitor_role NOT NULL
);

GRANT SELECT ON TABLE federated.competitor_component TO anonymous;

ALTER TABLE ONLY federated.competitor_component
    ADD CONSTRAINT competitor_component_pkey PRIMARY KEY (competitor_id, person_id);
ALTER TABLE ONLY federated.competitor_component
    ADD CONSTRAINT competitor_component_competitor_id_fkey FOREIGN KEY (competitor_id) REFERENCES federated.competitor(id) ON DELETE CASCADE;
ALTER TABLE ONLY federated.competitor_component
    ADD CONSTRAINT competitor_component_person_id_fkey FOREIGN KEY (person_id) REFERENCES federated.person(id);

CREATE INDEX competitor_component_person_id_idx ON federated.competitor_component USING btree (person_id);
