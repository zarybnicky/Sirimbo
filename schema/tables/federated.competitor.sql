CREATE TABLE federated.competitor (
    id bigint NOT NULL,
    competitor_type federated.competitor_type NOT NULL,
    name text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

ALTER TABLE ONLY federated.competitor
    ADD CONSTRAINT competitor_pkey PRIMARY KEY (id);
