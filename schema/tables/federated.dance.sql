CREATE TABLE federated.dance (
    code text NOT NULL,
    name text NOT NULL,
    discipline text NOT NULL
);

GRANT SELECT ON TABLE federated.dance TO anonymous;

ALTER TABLE ONLY federated.dance
    ADD CONSTRAINT dance_pkey PRIMARY KEY (code);
