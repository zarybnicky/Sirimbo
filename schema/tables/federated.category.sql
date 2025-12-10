CREATE TABLE federated.category (
    id bigint NOT NULL,
    name text NOT NULL,
    series text NOT NULL,
    discipline text NOT NULL,
    age_group text NOT NULL,
    gender_group text DEFAULT 'mixed'::text NOT NULL,
    class text NOT NULL,
    base_dance_program_id bigint
);

ALTER TABLE ONLY federated.category
    ADD CONSTRAINT category_pkey PRIMARY KEY (id);
ALTER TABLE ONLY federated.category
    ADD CONSTRAINT category_series_discipline_age_group_gender_group_class_key UNIQUE (series, discipline, age_group, gender_group, class);
ALTER TABLE ONLY federated.category
    ADD CONSTRAINT category_base_dance_program_id_fkey FOREIGN KEY (base_dance_program_id) REFERENCES federated.dance_program(id);
