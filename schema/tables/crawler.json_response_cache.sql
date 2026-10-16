CREATE TABLE crawler.json_response_cache (
    content_hash text GENERATED ALWAYS AS (encode(public.digest((content)::text, 'sha256'::text), 'hex'::text)) STORED NOT NULL,
    content jsonb NOT NULL
);

COMMENT ON TABLE crawler.json_response_cache IS '@name crawler_response_cache
@behavior -insert -update -delete -list';

GRANT SELECT ON TABLE crawler.json_response_cache TO system_admin;
ALTER TABLE crawler.json_response_cache ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY crawler.json_response_cache
    ADD CONSTRAINT json_response_cache_pkey PRIMARY KEY (content_hash);

CREATE POLICY system_admin_read ON crawler.json_response_cache FOR SELECT TO system_admin USING (true);
