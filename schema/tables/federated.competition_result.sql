CREATE TABLE federated.competition_result (
    competition_id bigint NOT NULL,
    competitor_id text NOT NULL,
    start_number text,
    ranking integer NOT NULL,
    ranking_to integer,
    point_gain numeric(10,3),
    final_gain numeric(10,3),
    is_final boolean,
    completion_status text,
    last_round text,
    last_dance text,
    club_id bigint,
    represents text,
    country text,
    CONSTRAINT competition_result_check CHECK (((ranking_to IS NULL) OR (ranking_to >= ranking)))
);

COMMENT ON TABLE federated.competition_result IS '@name competition_result
@behavior -insert -update -delete -list +query:resource:connection -query:resource:list -query:resource:single
@foreignKey (competition_id,last_round) references federated.competition_round (competition_id,round_key)|@fieldName round|@behavior -manyRelation:resource:connection -manyRelation:resource:list';

GRANT SELECT ON TABLE federated.competition_result TO anonymous;

ALTER TABLE ONLY federated.competition_result
    ADD CONSTRAINT competition_result_pkey PRIMARY KEY (competition_id, competitor_id);
ALTER TABLE ONLY federated.competition_result
    ADD CONSTRAINT competition_result_club_id_fkey FOREIGN KEY (club_id) REFERENCES federated.federation_club(id);
ALTER TABLE ONLY federated.competition_result
    ADD CONSTRAINT competition_result_competition_id_fkey FOREIGN KEY (competition_id) REFERENCES federated.competition(id);
ALTER TABLE ONLY federated.competition_result
    ADD CONSTRAINT competition_result_competitor_id_fkey FOREIGN KEY (competitor_id) REFERENCES federated.competitor(id);

CREATE TRIGGER _900_refresh_competitor_activity_delete AFTER DELETE ON federated.competition_result REFERENCING OLD TABLE AS oldtable FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_competitor_activity__refresh();
CREATE TRIGGER _900_refresh_competitor_activity_insert AFTER INSERT ON federated.competition_result REFERENCING NEW TABLE AS newtable FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_competitor_activity__refresh();
CREATE TRIGGER _900_refresh_competitor_activity_update AFTER UPDATE ON federated.competition_result REFERENCING OLD TABLE AS oldtable NEW TABLE AS newtable FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_competitor_activity__refresh();

CREATE INDEX competition_result_club_id_idx ON federated.competition_result USING btree (club_id);
CREATE INDEX competition_result_competition_id_ranking_idx ON federated.competition_result USING btree (competition_id, ranking);
CREATE INDEX competition_result_competitor_id_idx ON federated.competition_result USING btree (competitor_id);
