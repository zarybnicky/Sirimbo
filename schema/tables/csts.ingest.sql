CREATE TABLE csts.ingest (
    type text NOT NULL,
    url text NOT NULL,
    hash text NOT NULL,
    payload jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    checked_at timestamp with time zone DEFAULT now() NOT NULL
);

ALTER TABLE ONLY csts.ingest
    ADD CONSTRAINT ingest_pkey PRIMARY KEY (type, url, hash);
