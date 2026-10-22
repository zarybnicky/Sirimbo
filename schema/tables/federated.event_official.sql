CREATE TABLE federated.event_official (
    event_id bigint NOT NULL,
    person_id text NOT NULL,
    role federated.official_role NOT NULL,
    discipline text DEFAULT ''::text NOT NULL,
    grade text
);

COMMENT ON TABLE federated.event_official IS '@name federation_event_official
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
COMMENT ON COLUMN federated.event_official.role IS '@behavior +attribute:orderBy';

GRANT SELECT ON TABLE federated.event_official TO anonymous;

ALTER TABLE ONLY federated.event_official
    ADD CONSTRAINT event_official_pkey PRIMARY KEY (event_id, person_id, role, discipline);
ALTER TABLE ONLY federated.event_official
    ADD CONSTRAINT event_official_event_id_fkey FOREIGN KEY (event_id) REFERENCES federated.event(id) ON DELETE CASCADE;
ALTER TABLE ONLY federated.event_official
    ADD CONSTRAINT event_official_person_id_fkey FOREIGN KEY (person_id) REFERENCES federated.person(id);

CREATE INDEX event_official_event_id_role_idx ON federated.event_official USING btree (event_id, role);
CREATE INDEX event_official_person_id_idx ON federated.event_official USING btree (person_id);
