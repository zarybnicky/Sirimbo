CREATE TABLE federated.federation (
    code text NOT NULL,
    name text NOT NULL
);

GRANT SELECT ON TABLE federated.federation TO anonymous;

ALTER TABLE ONLY federated.federation
    ADD CONSTRAINT federation_pkey PRIMARY KEY (code);
