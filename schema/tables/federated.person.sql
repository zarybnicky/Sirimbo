CREATE TABLE federated.person (
    id text GENERATED ALWAYS AS (((federation || ':'::text) || (external_id)::text)) STORED NOT NULL,
    federation text NOT NULL,
    external_id bigint NOT NULL,
    canonical_name text,
    first_name text,
    last_name text,
    gender federated.gender,
    dob date,
    nationality text,
    age_group text,
    medical_checkup_expiration date,
    medical_checkup_type text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    search_name text GENERATED ALWAYS AS (app_private.normalize_name(COALESCE(canonical_name, public.immutable_concat_ws(' '::text, VARIADIC ARRAY[first_name, last_name])))) STORED
);

GRANT SELECT ON TABLE federated.person TO anonymous;

ALTER TABLE ONLY federated.person
    ADD CONSTRAINT person_federation_external_id_key UNIQUE (federation, external_id);
ALTER TABLE ONLY federated.person
    ADD CONSTRAINT person_pkey PRIMARY KEY (id);
ALTER TABLE ONLY federated.person
    ADD CONSTRAINT person_federation_fkey FOREIGN KEY (federation) REFERENCES federated.federation(code);

CREATE INDEX federated_person_csts_search_name_trgm_idx ON federated.person USING gin (search_name public.gin_trgm_ops) WHERE (federation = 'csts'::text);
CREATE INDEX idx_person_search_name_trgm ON federated.person USING gin (search_name public.gin_trgm_ops);
