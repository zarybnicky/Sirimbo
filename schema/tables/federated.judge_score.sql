CREATE TABLE federated.judge_score (
    federation text NOT NULL,
    event_date date NOT NULL,
    event_id bigint NOT NULL,
    competition_id bigint NOT NULL,
    category_id bigint NOT NULL,
    round_id bigint NOT NULL,
    dance_order integer NOT NULL,
    dance_code text NOT NULL,
    judge_person_id bigint NOT NULL,
    competitor_id bigint NOT NULL,
    component federated.score_component NOT NULL,
    score numeric(10,3) NOT NULL,
    raw_score text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

GRANT SELECT ON TABLE federated.judge_score TO anonymous;

ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_pkey PRIMARY KEY (round_id, dance_order, judge_person_id, competitor_id, component);
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_competition_id_category_id_fkey FOREIGN KEY (competition_id, category_id) REFERENCES federated.competition(id, category_id) ON UPDATE CASCADE;
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_competition_id_event_id_fkey FOREIGN KEY (competition_id, event_id) REFERENCES federated.competition(id, event_id) ON UPDATE CASCADE;
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_federation_competition_id_fkey FOREIGN KEY (federation, competition_id) REFERENCES federated.competition(federation, id) ON UPDATE CASCADE;
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_federation_competitor_id_fkey FOREIGN KEY (federation, competitor_id) REFERENCES federated.competitor(federation, external_id);
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_federation_judge_person_id_fkey FOREIGN KEY (federation, judge_person_id) REFERENCES federated.person(federation, external_id);
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_round_id_competition_id_fkey FOREIGN KEY (round_id, competition_id) REFERENCES federated.competition_round(id, competition_id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY federated.judge_score
    ADD CONSTRAINT judge_score_round_id_dance_order_dance_code_fkey FOREIGN KEY (round_id, dance_order, dance_code) REFERENCES federated.round_dance(round_id, dance_order, dance_code) ON DELETE CASCADE;

CREATE INDEX judge_score_federation_category_id_event_date_idx ON federated.judge_score USING btree (federation, category_id, event_date);
CREATE INDEX judge_score_federation_competitor_id_event_date_idx ON federated.judge_score USING btree (federation, competitor_id, event_date);
CREATE INDEX judge_score_federation_judge_person_id_event_date_idx ON federated.judge_score USING btree (federation, judge_person_id, event_date);
