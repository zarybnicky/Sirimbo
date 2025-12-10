CREATE TABLE federated.federation_category (
    federation text NOT NULL,
    external_id text,
    category_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

ALTER TABLE ONLY federated.federation_category
    ADD CONSTRAINT federation_category_federation_external_id_key UNIQUE (federation, external_id);
ALTER TABLE ONLY federated.federation_category
    ADD CONSTRAINT federation_category_pkey PRIMARY KEY (federation, category_id);
ALTER TABLE ONLY federated.federation_category
    ADD CONSTRAINT federation_category_category_id_fkey FOREIGN KEY (category_id) REFERENCES federated.category(id);
ALTER TABLE ONLY federated.federation_category
    ADD CONSTRAINT federation_category_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);

CREATE INDEX federation_category_category_id_idx ON federated.federation_category USING btree (category_id);
