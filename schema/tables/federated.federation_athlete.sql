CREATE TABLE federated.federation_athlete (
    federation text NOT NULL,
    external_id text NOT NULL,
    athlete_id bigint,
    age_group text,
    medical_checkup_expiration date,
    medical_checkup_type text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

GRANT SELECT ON TABLE federated.federation_athlete TO anonymous;

ALTER TABLE ONLY federated.federation_athlete
    ADD CONSTRAINT federation_athlete_pkey PRIMARY KEY (federation, external_id);
ALTER TABLE ONLY federated.federation_athlete
    ADD CONSTRAINT federation_athlete_athlete_id_fkey FOREIGN KEY (athlete_id) REFERENCES federated.athlete(id);
ALTER TABLE ONLY federated.federation_athlete
    ADD CONSTRAINT federation_athlete_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);

CREATE INDEX federation_athlete_athlete_id_idx ON federated.federation_athlete USING btree (athlete_id);
CREATE UNIQUE INDEX federation_athlete_federation_athlete_id_idx ON federated.federation_athlete USING btree (federation, athlete_id) WHERE (athlete_id IS NOT NULL);
