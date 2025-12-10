CREATE TABLE federated.round_judge (
    round_id bigint NOT NULL,
    judge_id bigint NOT NULL,
    is_shadow boolean DEFAULT false NOT NULL
);

ALTER TABLE ONLY federated.round_judge
    ADD CONSTRAINT round_judge_pkey PRIMARY KEY (round_id, judge_id);
ALTER TABLE ONLY federated.round_judge
    ADD CONSTRAINT round_judge_judge_id_fkey FOREIGN KEY (judge_id) REFERENCES federated.judge(id);
ALTER TABLE ONLY federated.round_judge
    ADD CONSTRAINT round_judge_round_id_fkey FOREIGN KEY (round_id) REFERENCES federated.competition_round(id);

CREATE INDEX round_judge_judge_id_idx ON federated.round_judge USING btree (judge_id);
