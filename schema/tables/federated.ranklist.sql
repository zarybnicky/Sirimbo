CREATE TABLE federated.ranklist (
    id bigint NOT NULL,
    federation text NOT NULL,
    category_id bigint NOT NULL,
    name text NOT NULL
);

GRANT SELECT ON TABLE federated.ranklist TO anonymous;

ALTER TABLE ONLY federated.ranklist
    ADD CONSTRAINT ranklist_federation_category_id_key UNIQUE (federation, category_id);
ALTER TABLE ONLY federated.ranklist
    ADD CONSTRAINT ranklist_pkey PRIMARY KEY (id);
ALTER TABLE ONLY federated.ranklist
    ADD CONSTRAINT ranklist_category_id_fkey FOREIGN KEY (category_id) REFERENCES federated.category(id);
ALTER TABLE ONLY federated.ranklist
    ADD CONSTRAINT ranklist_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);
