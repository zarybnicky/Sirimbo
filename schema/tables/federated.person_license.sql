CREATE TABLE federated.person_license (
    person_id text NOT NULL,
    federation text NOT NULL,
    kind federated.person_license_kind NOT NULL,
    discipline federated.person_license_discipline DEFAULT 'general'::federated.person_license_discipline NOT NULL,
    grade text,
    valid_until date,
    status federated.person_license_status DEFAULT 'unknown'::federated.person_license_status NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE federated.person_license IS '@name federation_person_license
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
COMMENT ON COLUMN federated.person_license.kind IS '@behavior +attribute:orderBy';
COMMENT ON COLUMN federated.person_license.discipline IS '@behavior +attribute:orderBy';

GRANT SELECT ON TABLE federated.person_license TO anonymous;

ALTER TABLE ONLY federated.person_license
    ADD CONSTRAINT person_license_pkey PRIMARY KEY (person_id, kind, discipline);
ALTER TABLE ONLY federated.person_license
    ADD CONSTRAINT person_license_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);
ALTER TABLE ONLY federated.person_license
    ADD CONSTRAINT person_license_person_id_fkey FOREIGN KEY (person_id) REFERENCES federated.person(id) ON DELETE CASCADE;

CREATE INDEX person_license_federation_person_id_idx ON federated.person_license USING btree (federation, person_id);
