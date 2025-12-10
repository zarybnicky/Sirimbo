CREATE TABLE crawler.html_response_cache (
    content_hash text GENERATED ALWAYS AS (encode(public.digest(content, 'sha256'::text), 'hex'::text)) STORED NOT NULL,
    content text
);

ALTER TABLE ONLY crawler.html_response_cache
    ADD CONSTRAINT html_response_cache_pkey PRIMARY KEY (content_hash);
