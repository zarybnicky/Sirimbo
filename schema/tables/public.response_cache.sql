CREATE TABLE public.response_cache (
    id bigint NOT NULL,
    url text NOT NULL,
    status integer NOT NULL,
    content text NOT NULL,
    content_type text NOT NULL,
    cached_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE public.response_cache IS '@omit';

GRANT ALL ON TABLE public.response_cache TO anonymous;

ALTER TABLE ONLY public.response_cache
    ADD CONSTRAINT response_cache_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.response_cache
    ADD CONSTRAINT response_cache_url_key UNIQUE (url);
