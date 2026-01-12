CREATE TABLE federated.person (
    id bigint NOT NULL,
    canonical_name text,
    first_name text,
    last_name text,
    search_name text GENERATED ALWAYS AS (federated.normalize_name(COALESCE(canonical_name, public.immutable_concat_ws(' '::text, VARIADIC ARRAY[first_name, last_name])))) STORED,
    gender federated.gender,
    dob date,
    nationality text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

GRANT SELECT ON TABLE federated.person TO anonymous;

ALTER TABLE ONLY federated.person
    ADD CONSTRAINT person_pkey PRIMARY KEY (id);
