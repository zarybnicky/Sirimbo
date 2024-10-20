CREATE TABLE public.pghero_space_stats (
    id bigint NOT NULL,
    database text,
    schema text,
    relation text,
    size bigint,
    captured_at timestamp without time zone
);

COMMENT ON TABLE public.pghero_space_stats IS '@omit';

ALTER TABLE ONLY public.pghero_space_stats
    ADD CONSTRAINT pghero_space_stats_pkey PRIMARY KEY (id);

CREATE INDEX pghero_space_stats_database_captured_at_idx ON public.pghero_space_stats USING btree (database, captured_at);
