CREATE TABLE federated.federation_judge (
    federation text NOT NULL,
    external_id text NOT NULL,
    judge_id bigint,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

GRANT SELECT ON TABLE federated.federation_judge TO anonymous;

ALTER TABLE ONLY federated.federation_judge
    ADD CONSTRAINT federation_judge_pkey PRIMARY KEY (federation, external_id);
ALTER TABLE ONLY federated.federation_judge
    ADD CONSTRAINT federation_judge_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);
ALTER TABLE ONLY federated.federation_judge
    ADD CONSTRAINT federation_judge_judge_id_fkey FOREIGN KEY (judge_id) REFERENCES federated.judge(id);

CREATE UNIQUE INDEX federation_judge_federation_judge_id_idx ON federated.federation_judge USING btree (federation, judge_id) WHERE (judge_id IS NOT NULL);
CREATE INDEX federation_judge_judge_id_idx ON federated.federation_judge USING btree (judge_id);
