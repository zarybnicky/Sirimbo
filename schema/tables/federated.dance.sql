CREATE TABLE federated.dance (
    code text NOT NULL,
    name text NOT NULL,
    discipline text NOT NULL
);

COMMENT ON TABLE federated.dance IS '@name dance
@behavior -insert -update -delete +list -connection';

GRANT SELECT ON TABLE federated.dance TO anonymous;

ALTER TABLE ONLY federated.dance
    ADD CONSTRAINT dance_pkey PRIMARY KEY (code);
