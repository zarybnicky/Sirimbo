CREATE TABLE federated.event_official (
    event_id bigint NOT NULL,
    person_id text NOT NULL,
    role federated.official_role NOT NULL,
    discipline text DEFAULT ''::text NOT NULL,
    grade text
);

GRANT SELECT ON TABLE federated.event_official TO anonymous;

ALTER TABLE ONLY federated.event_official
    ADD CONSTRAINT event_official_pkey PRIMARY KEY (event_id, person_id, role, discipline);
ALTER TABLE ONLY federated.event_official
    ADD CONSTRAINT event_official_event_id_fkey FOREIGN KEY (event_id) REFERENCES federated.event(id) ON DELETE CASCADE;
ALTER TABLE ONLY federated.event_official
    ADD CONSTRAINT event_official_person_id_fkey FOREIGN KEY (person_id) REFERENCES federated.person(id);

CREATE INDEX event_official_event_id_role_idx ON federated.event_official USING btree (event_id, role);
CREATE INDEX event_official_person_id_idx ON federated.event_official USING btree (person_id);
