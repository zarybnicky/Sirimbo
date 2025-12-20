CREATE TABLE federated.dance_program_dance (
    program_id bigint NOT NULL,
    dance_code text NOT NULL,
    dance_order integer NOT NULL
);

GRANT SELECT ON TABLE federated.dance_program_dance TO anonymous;

ALTER TABLE ONLY federated.dance_program_dance
    ADD CONSTRAINT dance_program_dance_pkey PRIMARY KEY (program_id, dance_code);
ALTER TABLE ONLY federated.dance_program_dance
    ADD CONSTRAINT dance_program_dance_program_id_dance_order_key UNIQUE (program_id, dance_order);
ALTER TABLE ONLY federated.dance_program_dance
    ADD CONSTRAINT dance_program_dance_dance_code_fkey FOREIGN KEY (dance_code) REFERENCES federated.dance(code);
ALTER TABLE ONLY federated.dance_program_dance
    ADD CONSTRAINT dance_program_dance_program_id_fkey FOREIGN KEY (program_id) REFERENCES federated.dance_program(id) ON DELETE CASCADE;

CREATE INDEX dance_program_dance_dance_code_idx ON federated.dance_program_dance USING btree (dance_code);
CREATE INDEX dance_program_dance_program_id_idx ON federated.dance_program_dance USING btree (program_id);
