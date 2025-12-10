CREATE TABLE federated.federation_competitor (
    federation text NOT NULL,
    external_id text NOT NULL,
    competitor_id bigint,
    age_group text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

ALTER TABLE ONLY federated.federation_competitor
    ADD CONSTRAINT federation_competitor_federation_competitor_id_key UNIQUE (federation, competitor_id);
ALTER TABLE ONLY federated.federation_competitor
    ADD CONSTRAINT federation_competitor_pkey PRIMARY KEY (federation, external_id);
ALTER TABLE ONLY federated.federation_competitor
    ADD CONSTRAINT federation_competitor_competitor_id_fkey FOREIGN KEY (competitor_id) REFERENCES federated.competitor(id);
ALTER TABLE ONLY federated.federation_competitor
    ADD CONSTRAINT federation_competitor_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);
