CREATE TABLE federated.couple_age_rule (
    age_group_code text NOT NULL,
    division text DEFAULT 'General'::text NOT NULL,
    older_age int4range NOT NULL,
    younger_age int4range NOT NULL,
    CONSTRAINT couple_age_rule_older_age_check CHECK ((NOT isempty(older_age))),
    CONSTRAINT couple_age_rule_younger_age_check CHECK ((NOT isempty(younger_age)))
);

COMMENT ON TABLE federated.couple_age_rule IS '@name federation_couple_age_rule
@behavior -insert -update -delete +list -connection -query:resource:single -query:resource:list -query:resource:connection';
COMMENT ON COLUMN federated.couple_age_rule.older_age IS '@behavior -filterBy -orderBy';
COMMENT ON COLUMN federated.couple_age_rule.younger_age IS '@behavior -filterBy -orderBy';

GRANT SELECT ON TABLE federated.couple_age_rule TO anonymous;

ALTER TABLE ONLY federated.couple_age_rule
    ADD CONSTRAINT couple_age_rule_pkey PRIMARY KEY (age_group_code, division);
ALTER TABLE ONLY federated.couple_age_rule
    ADD CONSTRAINT couple_age_rule_age_group_code_fkey FOREIGN KEY (age_group_code) REFERENCES federated.age_group(code);
