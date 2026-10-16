CREATE TABLE federated.age_group (
    code text NOT NULL,
    name text NOT NULL,
    aliases text[] DEFAULT '{}'::text[] NOT NULL,
    allowed_to_dance_in text[] DEFAULT '{}'::text[] NOT NULL,
    age int4range,
    person_age int4range,
    is_person boolean DEFAULT false NOT NULL,
    is_couple boolean DEFAULT false NOT NULL,
    is_team boolean DEFAULT false NOT NULL,
    is_competition boolean DEFAULT false NOT NULL,
    divisions text[] DEFAULT '{}'::text[] NOT NULL,
    CONSTRAINT age_group_age_check CHECK ((NOT isempty(age))),
    CONSTRAINT age_group_person_age_check CHECK ((NOT isempty(person_age)))
);

COMMENT ON TABLE federated.age_group IS '@name age_group
@behavior -insert -update -delete +list -connection';
COMMENT ON COLUMN federated.age_group.aliases IS '@behavior -filterBy -orderBy';
COMMENT ON COLUMN federated.age_group.allowed_to_dance_in IS '@behavior -filterBy -orderBy';
COMMENT ON COLUMN federated.age_group.age IS '@behavior -filterBy -orderBy';
COMMENT ON COLUMN federated.age_group.person_age IS '@behavior -filterBy -orderBy';
COMMENT ON COLUMN federated.age_group.divisions IS '@behavior -filterBy -orderBy';

GRANT SELECT ON TABLE federated.age_group TO anonymous;

ALTER TABLE ONLY federated.age_group
    ADD CONSTRAINT age_group_pkey PRIMARY KEY (code);
