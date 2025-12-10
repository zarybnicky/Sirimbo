CREATE TABLE crawler.html_response (
    id bigint NOT NULL,
    frontier_id bigint NOT NULL,
    url text NOT NULL,
    fetched_at timestamp with time zone DEFAULT now() NOT NULL,
    http_status integer,
    error text,
    content_hash text
);

ALTER TABLE ONLY crawler.html_response
    ADD CONSTRAINT html_response_pkey PRIMARY KEY (id);
ALTER TABLE ONLY crawler.html_response
    ADD CONSTRAINT html_response_content_hash_fkey FOREIGN KEY (content_hash) REFERENCES crawler.html_response_cache(content_hash);
ALTER TABLE ONLY crawler.html_response
    ADD CONSTRAINT html_response_frontier_id_fkey FOREIGN KEY (frontier_id) REFERENCES crawler.frontier(id) ON DELETE CASCADE;
