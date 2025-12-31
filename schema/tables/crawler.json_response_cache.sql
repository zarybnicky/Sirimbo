CREATE TABLE crawler.json_response_cache (
    content_hash text GENERATED ALWAYS AS (encode(public.digest((content)::text, 'sha256'::text), 'hex'::text)) STORED NOT NULL,
    content jsonb NOT NULL
);

ALTER TABLE ONLY crawler.json_response_cache
    ADD CONSTRAINT json_response_cache_pkey PRIMARY KEY (content_hash);
