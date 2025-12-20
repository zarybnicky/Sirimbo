CREATE TABLE federated.athlete (
    id bigint NOT NULL,
    person_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

GRANT SELECT ON TABLE federated.athlete TO anonymous;

ALTER TABLE ONLY federated.athlete
    ADD CONSTRAINT athlete_person_id_key UNIQUE (person_id);
ALTER TABLE ONLY federated.athlete
    ADD CONSTRAINT athlete_pkey PRIMARY KEY (id);
ALTER TABLE ONLY federated.athlete
    ADD CONSTRAINT athlete_person_id_fkey FOREIGN KEY (person_id) REFERENCES federated.person(id);
