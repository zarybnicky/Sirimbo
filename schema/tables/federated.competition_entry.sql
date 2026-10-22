CREATE TABLE federated.competition_entry (
    competition_id bigint NOT NULL,
    competitor_id text NOT NULL,
    cancelled boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE federated.competition_entry IS '@name competition_entry
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';

GRANT SELECT ON TABLE federated.competition_entry TO anonymous;

ALTER TABLE ONLY federated.competition_entry
    ADD CONSTRAINT competition_entry_pkey PRIMARY KEY (competition_id, competitor_id);
ALTER TABLE ONLY federated.competition_entry
    ADD CONSTRAINT competition_entry_competition_id_fkey FOREIGN KEY (competition_id) REFERENCES federated.competition(id);
ALTER TABLE ONLY federated.competition_entry
    ADD CONSTRAINT competition_entry_competitor_id_fkey FOREIGN KEY (competitor_id) REFERENCES federated.competitor(id);

CREATE TRIGGER _900_refresh_competitor_activity_delete AFTER DELETE ON federated.competition_entry REFERENCING OLD TABLE AS oldtable FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_competitor_activity__refresh();
CREATE TRIGGER _900_refresh_competitor_activity_insert AFTER INSERT ON federated.competition_entry REFERENCING NEW TABLE AS newtable FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_competitor_activity__refresh();
CREATE TRIGGER _900_refresh_competitor_activity_update AFTER UPDATE ON federated.competition_entry REFERENCING OLD TABLE AS oldtable NEW TABLE AS newtable FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_competitor_activity__refresh();

CREATE INDEX competition_entry_competition_id_idx ON federated.competition_entry USING btree (competition_id);
CREATE INDEX competition_entry_competitor_id_idx ON federated.competition_entry USING btree (competitor_id);
