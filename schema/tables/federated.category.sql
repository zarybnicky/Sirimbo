CREATE TABLE federated.category (
    id bigint NOT NULL,
    name text NOT NULL,
    series text NOT NULL,
    discipline text NOT NULL,
    age_group text NOT NULL,
    gender_group text DEFAULT 'mixed'::text NOT NULL,
    class text NOT NULL,
    competitor_type federated.competitor_type DEFAULT 'couple'::federated.competitor_type NOT NULL,
    base_dance_program_id bigint,
    age_group_code text
);

COMMENT ON TABLE federated.category IS '@name category
@behavior -insert -update -delete +list -connection';
COMMENT ON COLUMN federated.category.age_group IS '@behavior -*';
COMMENT ON COLUMN federated.category.age_group_code IS '@name ageGroup
@behavior -insert -update';

GRANT SELECT ON TABLE federated.category TO anonymous;

ALTER TABLE ONLY federated.category
    ADD CONSTRAINT category_pkey PRIMARY KEY (id);
ALTER TABLE ONLY federated.category
    ADD CONSTRAINT category_series_discipline_age_group_gender_group_class_com_key UNIQUE (series, discipline, age_group, gender_group, class, competitor_type);
ALTER TABLE ONLY federated.category
    ADD CONSTRAINT category_age_group_code_fkey FOREIGN KEY (age_group_code) REFERENCES federated.age_group(code);
ALTER TABLE ONLY federated.category
    ADD CONSTRAINT category_base_dance_program_id_fkey FOREIGN KEY (base_dance_program_id) REFERENCES federated.dance_program(id);

CREATE TRIGGER _resolve_age_group BEFORE INSERT OR UPDATE OF age_group ON federated.category FOR EACH ROW EXECUTE FUNCTION app_private.tg_resolve_age_group();

CREATE INDEX category_age_group_code_idx ON federated.category USING btree (age_group_code);
