CREATE TABLE federated.round_dance (
    round_id bigint NOT NULL,
    dance_program_id bigint NOT NULL,
    dance_code text NOT NULL,
    dance_order integer NOT NULL
);

GRANT SELECT ON TABLE federated.round_dance TO anonymous;

ALTER TABLE ONLY federated.round_dance
    ADD CONSTRAINT round_dance_pkey PRIMARY KEY (round_id, dance_order);
ALTER TABLE ONLY federated.round_dance
    ADD CONSTRAINT round_dance_round_id_dance_order_dance_code_key UNIQUE (round_id, dance_order, dance_code);
ALTER TABLE ONLY federated.round_dance
    ADD CONSTRAINT round_dance_dance_program_id_dance_code_fkey FOREIGN KEY (dance_program_id, dance_code) REFERENCES federated.dance_program_dance(program_id, dance_code);
ALTER TABLE ONLY federated.round_dance
    ADD CONSTRAINT round_dance_round_id_dance_program_id_fkey FOREIGN KEY (round_id, dance_program_id) REFERENCES federated.competition_round(id, dance_program_id) ON DELETE CASCADE;
