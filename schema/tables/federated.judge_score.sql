CREATE TABLE federated.judge_score (
    round_dance_id bigint NOT NULL,
    judge_id bigint NOT NULL,
    competitor_id bigint NOT NULL,
    component federated.score_component NOT NULL,
    score numeric(10,3) NOT NULL,
    raw_score text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_pkey PRIMARY KEY (round_dance_id, judge_id, competitor_id, component);
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_competitor_id_fkey FOREIGN KEY (competitor_id) REFERENCES federated.competitor(id);
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_judge_id_fkey FOREIGN KEY (judge_id) REFERENCES federated.judge(id);
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_round_dance_id_fkey FOREIGN KEY (round_dance_id) REFERENCES federated.round_dance(id);

CREATE INDEX judge_score_competitor_id_idx ON federated.judge_score USING btree (competitor_id);
CREATE INDEX judge_score_judge_id_idx ON federated.judge_score USING btree (judge_id);
CREATE INDEX judge_score_round_dance_id_idx ON federated.judge_score USING btree (round_dance_id);
