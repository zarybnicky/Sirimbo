CREATE TABLE crawler.frontier (
    id bigint NOT NULL,
    federation text NOT NULL,
    kind text NOT NULL,
    key text NOT NULL,
    discovered_at timestamp with time zone DEFAULT now() NOT NULL,
    last_fetched_at timestamp with time zone,
    fetch_status crawler.fetch_status DEFAULT 'pending'::crawler.fetch_status NOT NULL,
    process_status crawler.process_status DEFAULT 'pending'::crawler.process_status NOT NULL,
    error_count integer DEFAULT 0 NOT NULL,
    next_fetch_at timestamp with time zone,
    meta jsonb DEFAULT '{}'::jsonb NOT NULL
);

ALTER TABLE ONLY crawler.frontier
    ADD CONSTRAINT frontier_federation_kind_key_key UNIQUE (federation, kind, key);
ALTER TABLE ONLY crawler.frontier
    ADD CONSTRAINT frontier_pkey PRIMARY KEY (id);

CREATE INDEX frontier_federation_kind_idx ON crawler.frontier USING btree (federation, kind);
CREATE INDEX frontier_federation_kind_next_fetch_at_idx ON crawler.frontier USING btree (federation, kind, next_fetch_at) WHERE (fetch_status = 'pending'::crawler.fetch_status);
