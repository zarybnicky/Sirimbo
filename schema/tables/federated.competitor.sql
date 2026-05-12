CREATE TABLE federated.competitor (
    id text GENERATED ALWAYS AS (((federation || ':'::text) || (external_id)::text)) STORED NOT NULL,
    federation text NOT NULL,
    external_id bigint NOT NULL,
    competitor_type federated.competitor_type NOT NULL,
    age_group text,
    name text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

GRANT SELECT ON TABLE federated.competitor TO anonymous;

ALTER TABLE ONLY federated.competitor
    ADD CONSTRAINT competitor_federation_external_id_key UNIQUE (federation, external_id);
ALTER TABLE ONLY federated.competitor
    ADD CONSTRAINT competitor_pkey PRIMARY KEY (id);
ALTER TABLE ONLY federated.competitor
    ADD CONSTRAINT competitor_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);
