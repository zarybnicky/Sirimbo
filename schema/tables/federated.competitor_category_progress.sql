CREATE TABLE federated.competitor_category_progress (
    federation text NOT NULL,
    competitor_id bigint NOT NULL,
    category_id bigint NOT NULL,
    points numeric(10,3) DEFAULT 0 NOT NULL,
    domestic_finale integer DEFAULT 0 NOT NULL,
    foreign_finale integer DEFAULT 0 NOT NULL
);

ALTER TABLE ONLY federated.competitor_category_progress
    ADD CONSTRAINT competitor_category_progress_pkey PRIMARY KEY (federation, competitor_id, category_id);
ALTER TABLE ONLY federated.competitor_category_progress
    ADD CONSTRAINT competitor_category_progress_category_id_fkey FOREIGN KEY (category_id) REFERENCES federated.category(id);
ALTER TABLE ONLY federated.competitor_category_progress
    ADD CONSTRAINT competitor_category_progress_competitor_id_fkey FOREIGN KEY (competitor_id) REFERENCES federated.competitor(id);
ALTER TABLE ONLY federated.competitor_category_progress
    ADD CONSTRAINT competitor_category_progress_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);

CREATE INDEX competitor_category_progress_competitor_id_idx ON federated.competitor_category_progress USING btree (competitor_id);
