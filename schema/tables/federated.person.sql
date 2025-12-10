CREATE TABLE federated.person (
    id bigint NOT NULL,
    canonical_name text,
    first_name text,
    last_name text,
    search_name text GENERATED ALWAYS AS (federated.unaccent(COALESCE(canonical_name, ((first_name || ' '::text) || last_name)))) STORED,
    gender federated.gender,
    dob date,
    nationality text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

ALTER TABLE ONLY federated.person
    ADD CONSTRAINT person_pkey PRIMARY KEY (id);
