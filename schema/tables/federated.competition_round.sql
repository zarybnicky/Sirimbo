CREATE TABLE federated.competition_round (
    id bigint NOT NULL,
    competition_id bigint NOT NULL,
    round_label text,
    round_key text NOT NULL,
    round_index integer,
    dance_program_id bigint NOT NULL,
    scoring_method federated.scoring_method NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

GRANT SELECT ON TABLE federated.competition_round TO anonymous;

ALTER TABLE ONLY federated.competition_round
    ADD CONSTRAINT competition_round_competition_id_round_key_key UNIQUE (competition_id, round_key);
ALTER TABLE ONLY federated.competition_round
    ADD CONSTRAINT competition_round_id_competition_id_key UNIQUE (id, competition_id);
ALTER TABLE ONLY federated.competition_round
    ADD CONSTRAINT competition_round_pkey PRIMARY KEY (id);
ALTER TABLE ONLY federated.competition_round
    ADD CONSTRAINT competition_round_competition_id_fkey FOREIGN KEY (competition_id) REFERENCES federated.competition(id);
ALTER TABLE ONLY federated.competition_round
    ADD CONSTRAINT competition_round_dance_program_id_fkey FOREIGN KEY (dance_program_id) REFERENCES federated.dance_program(id);

CREATE INDEX competition_round_competition_id_round_key_idx ON federated.competition_round USING btree (competition_id, round_key);
CREATE INDEX competition_round_dance_program_id_idx ON federated.competition_round USING btree (dance_program_id);
