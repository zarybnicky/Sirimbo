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
    meta jsonb DEFAULT '{}'::jsonb NOT NULL,
    last_process_error text,
    last_process_error_at timestamp with time zone
);

ALTER TABLE ONLY crawler.frontier
    ADD CONSTRAINT frontier_federation_kind_key_key UNIQUE (federation, kind, key);
ALTER TABLE ONLY crawler.frontier
    ADD CONSTRAINT frontier_pkey PRIMARY KEY (id);

CREATE INDEX frontier_federation_kind_idx ON crawler.frontier USING btree (federation, kind);
CREATE INDEX frontier_federation_kind_next_fetch_at_idx ON crawler.frontier USING btree (federation, kind, next_fetch_at) WHERE ((fetch_status = ANY (ARRAY['pending'::crawler.fetch_status, 'transient'::crawler.fetch_status])) OR ((fetch_status = 'ok'::crawler.fetch_status) AND (process_status = 'ok'::crawler.process_status)));
CREATE INDEX frontier_process_pending_ok_pick_idx ON crawler.frontier USING btree (last_fetched_at, discovered_at, id) WHERE ((process_status = 'pending'::crawler.process_status) AND (fetch_status = 'ok'::crawler.fetch_status));
