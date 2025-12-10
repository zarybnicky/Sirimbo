CREATE TABLE federated.competition_entry (
    competition_id bigint NOT NULL,
    competitor_id bigint NOT NULL,
    cancelled boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

ALTER TABLE ONLY federated.competition_entry
    ADD CONSTRAINT competition_entry_pkey PRIMARY KEY (competition_id, competitor_id);
ALTER TABLE ONLY federated.competition_entry
    ADD CONSTRAINT competition_entry_competition_id_fkey FOREIGN KEY (competition_id) REFERENCES federated.competition(id);
ALTER TABLE ONLY federated.competition_entry
    ADD CONSTRAINT competition_entry_competitor_id_fkey FOREIGN KEY (competitor_id) REFERENCES federated.competitor(id);

CREATE INDEX competition_entry_competitor_id_idx ON federated.competition_entry USING btree (competitor_id);
