CREATE TABLE crawler.incremental_ranges (
    federation text NOT NULL,
    kind text NOT NULL,
    last_known integer DEFAULT 0 NOT NULL,
    last_checked integer DEFAULT 0 NOT NULL
);

ALTER TABLE ONLY crawler.incremental_ranges
    ADD CONSTRAINT incremental_ranges_pkey PRIMARY KEY (federation, kind);
