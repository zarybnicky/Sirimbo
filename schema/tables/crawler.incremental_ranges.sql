CREATE TABLE crawler.incremental_ranges (
    federation text NOT NULL,
    kind text NOT NULL,
    last_known integer DEFAULT 0 NOT NULL,
    last_checked integer DEFAULT 0 NOT NULL
);

COMMENT ON TABLE crawler.incremental_ranges IS '@name crawler_incremental_range
@behavior -insert -update -delete -list';

GRANT SELECT ON TABLE crawler.incremental_ranges TO system_admin;
ALTER TABLE crawler.incremental_ranges ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY crawler.incremental_ranges
    ADD CONSTRAINT incremental_ranges_pkey PRIMARY KEY (federation, kind);

CREATE POLICY system_admin_read ON crawler.incremental_ranges FOR SELECT TO system_admin USING (true);
