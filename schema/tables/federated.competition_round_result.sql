CREATE TABLE federated.competition_round_result (
    round_id bigint NOT NULL,
    competitor_id bigint NOT NULL,
    overall_ranking integer NOT NULL,
    overall_ranking_to integer,
    qualified_next boolean,
    overall_score numeric(10,3),
    CONSTRAINT competition_round_result_check CHECK (((overall_ranking_to IS NULL) OR (overall_ranking_to >= overall_ranking)))
);

GRANT SELECT ON TABLE federated.competition_round_result TO anonymous;

ALTER TABLE ONLY federated.competition_round_result
    ADD CONSTRAINT competition_round_result_pkey PRIMARY KEY (round_id, competitor_id);
ALTER TABLE ONLY federated.competition_round_result
    ADD CONSTRAINT competition_round_result_competitor_id_fkey FOREIGN KEY (competitor_id) REFERENCES federated.competitor(id);
ALTER TABLE ONLY federated.competition_round_result
    ADD CONSTRAINT competition_round_result_round_id_fkey FOREIGN KEY (round_id) REFERENCES federated.competition_round(id);

CREATE INDEX competition_round_result_competitor_id_idx ON federated.competition_round_result USING btree (competitor_id);
CREATE INDEX competition_round_result_round_id_overall_ranking_idx ON federated.competition_round_result USING btree (round_id, overall_ranking);
