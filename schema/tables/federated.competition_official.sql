CREATE TABLE federated.competition_official (
    competition_id bigint NOT NULL,
    person_id text NOT NULL,
    role federated.official_role NOT NULL,
    external_id text
);

GRANT SELECT ON TABLE federated.competition_official TO anonymous;

ALTER TABLE ONLY federated.competition_official
    ADD CONSTRAINT competition_official_pkey PRIMARY KEY (competition_id, person_id, role);
ALTER TABLE ONLY federated.competition_official
    ADD CONSTRAINT competition_official_competition_id_fkey FOREIGN KEY (competition_id) REFERENCES federated.competition(id) ON DELETE CASCADE;
ALTER TABLE ONLY federated.competition_official
    ADD CONSTRAINT competition_official_person_id_fkey FOREIGN KEY (person_id) REFERENCES federated.person(id);

CREATE INDEX competition_official_competition_id_external_id_idx ON federated.competition_official USING btree (competition_id, external_id) WHERE (external_id IS NOT NULL);
CREATE INDEX competition_official_competition_id_role_idx ON federated.competition_official USING btree (competition_id, role);
CREATE INDEX competition_official_person_id_idx ON federated.competition_official USING btree (person_id);
