CREATE TABLE federated.federation_club (
    id bigint NOT NULL,
    federation text NOT NULL,
    external_id text NOT NULL,
    name text NOT NULL,
    city text,
    country text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

GRANT SELECT ON TABLE federated.federation_club TO anonymous;

ALTER TABLE ONLY federated.federation_club
    ADD CONSTRAINT federation_club_federation_external_id_key UNIQUE (federation, external_id);
ALTER TABLE ONLY federated.federation_club
    ADD CONSTRAINT federation_club_federation_id_key UNIQUE (federation, id);
ALTER TABLE ONLY federated.federation_club
    ADD CONSTRAINT federation_club_pkey PRIMARY KEY (id);
ALTER TABLE ONLY federated.federation_club
    ADD CONSTRAINT federation_club_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);
