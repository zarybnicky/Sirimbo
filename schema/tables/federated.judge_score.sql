CREATE TABLE federated.judge_score (
    round_id bigint NOT NULL,
    dance_order integer NOT NULL,
    dance_code text NOT NULL,
    judge_person_id bigint NOT NULL,
    competitor_id text NOT NULL,
    component federated.score_component NOT NULL,
    score numeric(10,3) NOT NULL,
    raw_score text
);

COMMENT ON TABLE federated.judge_score IS '@name judge_score
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
COMMENT ON COLUMN federated.judge_score.component IS '@behavior +attribute:orderBy';

GRANT SELECT ON TABLE federated.judge_score TO anonymous;

ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_pkey PRIMARY KEY (round_id, dance_order, judge_person_id, competitor_id, component);
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_round_id_dance_order_dance_code_fkey FOREIGN KEY (round_id, dance_order, dance_code) REFERENCES federated.round_dance(round_id, dance_order, dance_code) ON DELETE CASCADE;
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_round_id_fkey FOREIGN KEY (round_id) REFERENCES federated.competition_round(id) ON UPDATE CASCADE ON DELETE CASCADE;
