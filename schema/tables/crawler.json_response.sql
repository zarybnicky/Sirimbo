CREATE TABLE crawler.json_response (
    id bigint NOT NULL,
    frontier_id bigint NOT NULL,
    url text NOT NULL,
    fetched_at timestamp with time zone DEFAULT now() NOT NULL,
    http_status integer,
    error text,
    content_hash text,
    sys_period tstzrange,
    last_observed_at timestamp with time zone,
    CONSTRAINT json_response_observation_check CHECK ((((sys_period IS NULL) AND (last_observed_at IS NULL)) OR ((sys_period IS NOT NULL) AND (last_observed_at IS NOT NULL) AND (NOT isempty(sys_period)) AND (NOT lower_inf(sys_period)) AND lower_inc(sys_period) AND (NOT upper_inc(sys_period)) AND (lower(sys_period) = fetched_at) AND (sys_period @> last_observed_at))))
);

ALTER TABLE ONLY crawler.json_response
    ADD CONSTRAINT json_response_pkey PRIMARY KEY (id);
ALTER TABLE ONLY crawler.json_response
    ADD CONSTRAINT json_response_content_hash_fkey FOREIGN KEY (content_hash) REFERENCES crawler.json_response_cache(content_hash);
ALTER TABLE ONLY crawler.json_response
    ADD CONSTRAINT json_response_frontier_id_fkey FOREIGN KEY (frontier_id) REFERENCES crawler.frontier(id) ON DELETE CASCADE;

CREATE INDEX json_response_content_hash_idx ON crawler.json_response USING btree (content_hash);
CREATE UNIQUE INDEX json_response_current_frontier_idx ON crawler.json_response USING btree (frontier_id) WHERE upper_inf(sys_period);
CREATE INDEX json_response_frontier_fetched_desc_idx ON crawler.json_response USING btree (frontier_id, fetched_at DESC, id DESC);
