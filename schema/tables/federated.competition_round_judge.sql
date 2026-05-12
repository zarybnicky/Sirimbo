CREATE TABLE federated.competition_round_judge (
    round_id bigint NOT NULL,
    person_judge_id text NOT NULL,
    judge_index integer NOT NULL,
    judge_label text
);

GRANT SELECT ON TABLE federated.competition_round_judge TO anonymous;

ALTER TABLE ONLY federated.competition_round_judge
    ADD CONSTRAINT competition_round_judge_pkey PRIMARY KEY (round_id, person_judge_id);
ALTER TABLE ONLY federated.competition_round_judge
    ADD CONSTRAINT competition_round_judge_round_id_judge_index_key UNIQUE (round_id, judge_index);
ALTER TABLE ONLY federated.competition_round_judge
    ADD CONSTRAINT competition_round_judge_person_judge_id_fkey FOREIGN KEY (person_judge_id) REFERENCES federated.person(id);
ALTER TABLE ONLY federated.competition_round_judge
    ADD CONSTRAINT competition_round_judge_round_id_fkey FOREIGN KEY (round_id) REFERENCES federated.competition_round(id) ON DELETE CASCADE;

CREATE INDEX competition_round_judge_judge ON federated.competition_round_judge USING btree (person_judge_id);
