CREATE TABLE federated.competition_result (
    competition_id bigint NOT NULL,
    competitor_id bigint NOT NULL,
    start_number text,
    ranking integer NOT NULL,
    ranking_to integer,
    point_gain numeric(10,3),
    final_gain numeric(10,3),
    CONSTRAINT competition_result_check CHECK (((ranking_to IS NULL) OR (ranking_to >= ranking)))
);

GRANT SELECT ON TABLE federated.competition_result TO anonymous;

ALTER TABLE ONLY federated.competition_result
    ADD CONSTRAINT competition_result_competition_id_start_number_key UNIQUE (competition_id, start_number);
ALTER TABLE ONLY federated.competition_result
    ADD CONSTRAINT competition_result_pkey PRIMARY KEY (competition_id, competitor_id);
ALTER TABLE ONLY federated.competition_result
    ADD CONSTRAINT competition_result_competition_id_fkey FOREIGN KEY (competition_id) REFERENCES federated.competition(id);
ALTER TABLE ONLY federated.competition_result
    ADD CONSTRAINT competition_result_competitor_id_fkey FOREIGN KEY (competitor_id) REFERENCES federated.competitor(id);

CREATE INDEX competition_result_competition_id_ranking_idx ON federated.competition_result USING btree (competition_id, ranking);
CREATE INDEX competition_result_competitor_id_idx ON federated.competition_result USING btree (competitor_id);
