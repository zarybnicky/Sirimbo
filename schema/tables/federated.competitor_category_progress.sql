CREATE TABLE federated.competitor_category_progress (
    competitor_id text NOT NULL,
    category_id bigint NOT NULL,
    points numeric(10,3) DEFAULT 0 NOT NULL,
    domestic_finals integer DEFAULT 0 NOT NULL,
    foreign_finals integer DEFAULT 0 NOT NULL
);

COMMENT ON TABLE federated.competitor_category_progress IS '@name federation_competitor_category_progress
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';

GRANT SELECT ON TABLE federated.competitor_category_progress TO anonymous;

ALTER TABLE ONLY federated.competitor_category_progress
    ADD CONSTRAINT competitor_category_progress_pkey PRIMARY KEY (competitor_id, category_id);
ALTER TABLE ONLY federated.competitor_category_progress
    ADD CONSTRAINT competitor_category_progress_category_id_fkey FOREIGN KEY (category_id) REFERENCES federated.category(id);
ALTER TABLE ONLY federated.competitor_category_progress
    ADD CONSTRAINT competitor_category_progress_competitor_id_fkey FOREIGN KEY (competitor_id) REFERENCES federated.competitor(id);

CREATE INDEX competitor_category_progress_competitor_id_idx ON federated.competitor_category_progress USING btree (competitor_id);
