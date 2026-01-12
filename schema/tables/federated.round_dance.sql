CREATE TABLE federated.round_dance (
    round_id bigint NOT NULL,
    dance_code text NOT NULL
);

GRANT SELECT ON TABLE federated.round_dance TO anonymous;

ALTER TABLE ONLY federated.round_dance
    ADD CONSTRAINT round_dance_pkey PRIMARY KEY (round_id, dance_code);
ALTER TABLE ONLY federated.round_dance
    ADD CONSTRAINT round_dance_dance_code_fkey FOREIGN KEY (dance_code) REFERENCES federated.dance(code);
ALTER TABLE ONLY federated.round_dance
    ADD CONSTRAINT round_dance_round_id_fkey FOREIGN KEY (round_id) REFERENCES federated.competition_round(id);

CREATE TRIGGER _100_round_dance__dance_program BEFORE INSERT ON federated.round_dance FOR EACH ROW EXECUTE FUNCTION federated.tg_round_dance__dance_program();

CREATE INDEX round_dance_round_id_idx ON federated.round_dance USING btree (round_id);
