CREATE TABLE federated.competition_round (
    id bigint NOT NULL,
    competition_id bigint NOT NULL,
    round_index integer NOT NULL,
    round_type text,
    dance_program_id bigint NOT NULL,
    scoring_method federated.scoring_method NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

ALTER TABLE ONLY federated.competition_round
    ADD CONSTRAINT competition_round_competition_id_round_index_key UNIQUE (competition_id, round_index);
ALTER TABLE ONLY federated.competition_round
    ADD CONSTRAINT competition_round_pkey PRIMARY KEY (id);
ALTER TABLE ONLY federated.competition_round
    ADD CONSTRAINT competition_round_competition_id_fkey FOREIGN KEY (competition_id) REFERENCES federated.competition(id);
ALTER TABLE ONLY federated.competition_round
    ADD CONSTRAINT competition_round_dance_program_id_fkey FOREIGN KEY (dance_program_id) REFERENCES federated.dance_program(id);

CREATE INDEX competition_round_competition_id_round_index_idx ON federated.competition_round USING btree (competition_id, round_index);
CREATE INDEX competition_round_dance_program_id_idx ON federated.competition_round USING btree (dance_program_id);
