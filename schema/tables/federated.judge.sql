CREATE TABLE federated.judge (
    id bigint NOT NULL,
    person_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

ALTER TABLE ONLY federated.judge
    ADD CONSTRAINT judge_person_id_key UNIQUE (person_id);
ALTER TABLE ONLY federated.judge
    ADD CONSTRAINT judge_pkey PRIMARY KEY (id);
ALTER TABLE ONLY federated.judge
    ADD CONSTRAINT judge_person_id_fkey FOREIGN KEY (person_id) REFERENCES federated.person(id);
