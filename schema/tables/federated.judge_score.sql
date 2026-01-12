CREATE TABLE federated.judge_score (
    federation text NOT NULL,
    event_date date NOT NULL,
    event_id bigint NOT NULL,
    competition_id bigint NOT NULL,
    category_id bigint NOT NULL,
    round_id bigint NOT NULL,
    dance_code text NOT NULL,
    judge_id bigint NOT NULL,
    competitor_id bigint NOT NULL,
    component federated.score_component NOT NULL,
    score numeric(10,3) NOT NULL,
    raw_score text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

GRANT SELECT ON TABLE federated.judge_score TO anonymous;

ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_pkey PRIMARY KEY (round_id, dance_code, judge_id, competitor_id, component);
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_category_id_fkey FOREIGN KEY (category_id) REFERENCES federated.category(id);
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_competition_id_category_id_fkey FOREIGN KEY (competition_id, category_id) REFERENCES federated.competition(id, category_id);
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_competition_id_event_id_fkey FOREIGN KEY (competition_id, event_id) REFERENCES federated.competition(id, event_id);
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_competition_id_fkey FOREIGN KEY (competition_id) REFERENCES federated.competition(id);
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_competitor_id_fkey FOREIGN KEY (competitor_id) REFERENCES federated.competitor(id);
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_dance_code_fkey FOREIGN KEY (dance_code) REFERENCES federated.dance(code);
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_event_id_fkey FOREIGN KEY (event_id) REFERENCES federated.event(id);
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_federation_competition_id_fkey FOREIGN KEY (federation, competition_id) REFERENCES federated.competition(federation, id);
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_judge_id_fkey FOREIGN KEY (judge_id) REFERENCES federated.judge(id);
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_round_id_competition_id_fkey FOREIGN KEY (round_id, competition_id) REFERENCES federated.competition_round(id, competition_id);
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_round_id_dance_code_fkey FOREIGN KEY (round_id, dance_code) REFERENCES federated.round_dance(round_id, dance_code);
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_round_id_fkey FOREIGN KEY (round_id) REFERENCES federated.competition_round(id);

CREATE INDEX judge_score_federation_category_id_event_date_idx ON federated.judge_score USING btree (federation, category_id, event_date);
CREATE INDEX judge_score_federation_competitor_id_event_date_idx ON federated.judge_score USING btree (federation, competitor_id, event_date);
CREATE INDEX judge_score_federation_judge_id_event_date_idx ON federated.judge_score USING btree (federation, judge_id, event_date);
