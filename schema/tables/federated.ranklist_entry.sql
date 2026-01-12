CREATE TABLE federated.ranklist_entry (
    snapshot_id bigint NOT NULL,
    competitor_id bigint NOT NULL,
    ranking integer NOT NULL,
    ranking_to integer,
    points numeric(10,3),
    CONSTRAINT ranklist_entry_check CHECK (((ranking_to IS NULL) OR (ranking_to >= ranking)))
);

GRANT SELECT ON TABLE federated.ranklist_entry TO anonymous;

ALTER TABLE ONLY federated.ranklist_entry
    ADD CONSTRAINT ranklist_entry_pkey PRIMARY KEY (snapshot_id, competitor_id);
ALTER TABLE ONLY federated.ranklist_entry
    ADD CONSTRAINT ranklist_entry_competitor_id_fkey FOREIGN KEY (competitor_id) REFERENCES federated.competitor(id);
ALTER TABLE ONLY federated.ranklist_entry
    ADD CONSTRAINT ranklist_entry_snapshot_id_fkey FOREIGN KEY (snapshot_id) REFERENCES federated.ranklist_snapshot(id);

CREATE INDEX ranklist_entry_competitor_id_idx ON federated.ranklist_entry USING btree (competitor_id);
CREATE INDEX ranklist_entry_snapshot_id_ranking_idx ON federated.ranklist_entry USING btree (snapshot_id, ranking);
