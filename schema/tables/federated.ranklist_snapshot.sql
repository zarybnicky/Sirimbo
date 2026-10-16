CREATE TABLE federated.ranklist_snapshot (
    id bigint NOT NULL,
    ranklist_id bigint NOT NULL,
    kind text DEFAULT 'default'::text NOT NULL,
    as_of_date date NOT NULL
);

COMMENT ON TABLE federated.ranklist_snapshot IS '@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';

GRANT SELECT ON TABLE federated.ranklist_snapshot TO anonymous;

ALTER TABLE ONLY federated.ranklist_snapshot
    ADD CONSTRAINT ranklist_snapshot_pkey PRIMARY KEY (id);
ALTER TABLE ONLY federated.ranklist_snapshot
    ADD CONSTRAINT ranklist_snapshot_ranklist_id_as_of_date_kind_key UNIQUE (ranklist_id, as_of_date, kind);
ALTER TABLE ONLY federated.ranklist_snapshot
    ADD CONSTRAINT ranklist_snapshot_ranklist_id_fkey FOREIGN KEY (ranklist_id) REFERENCES federated.ranklist(id);
