CREATE TABLE federated.federation (
    code text NOT NULL,
    name text NOT NULL
);

ALTER TABLE ONLY federated.federation
    ADD CONSTRAINT federation_pkey PRIMARY KEY (code);
