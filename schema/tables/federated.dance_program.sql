CREATE TABLE federated.dance_program (
    id bigint NOT NULL,
    code text,
    name text NOT NULL,
    discipline text,
    is_default boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

GRANT SELECT ON TABLE federated.dance_program TO anonymous;

ALTER TABLE ONLY federated.dance_program
    ADD CONSTRAINT dance_program_code_key UNIQUE (code);
ALTER TABLE ONLY federated.dance_program
    ADD CONSTRAINT dance_program_pkey PRIMARY KEY (id);
