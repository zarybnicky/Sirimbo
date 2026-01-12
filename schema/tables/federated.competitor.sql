CREATE TABLE federated.competitor (
    id bigint NOT NULL,
    competitor_type federated.competitor_type NOT NULL,
    name text,
    component_sig text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

GRANT SELECT ON TABLE federated.competitor TO anonymous;

ALTER TABLE ONLY federated.competitor
    ADD CONSTRAINT competitor_competitor_type_component_sig_key UNIQUE (competitor_type, component_sig);
ALTER TABLE ONLY federated.competitor
    ADD CONSTRAINT competitor_pkey PRIMARY KEY (id);
