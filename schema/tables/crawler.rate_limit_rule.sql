CREATE TABLE crawler.rate_limit_rule (
    host text NOT NULL,
    max_requests integer NOT NULL,
    per_interval interval NOT NULL,
    spacing interval GENERATED ALWAYS AS (((per_interval / (max_requests)::double precision) + '00:00:00.02'::interval)) STORED NOT NULL,
    next_available_at timestamp with time zone DEFAULT '1970-01-01 00:00:00+01'::timestamp with time zone NOT NULL,
    CONSTRAINT rate_limit_rule_max_requests_check CHECK ((max_requests > 0)),
    CONSTRAINT rate_limit_rule_per_interval_check CHECK ((per_interval > '00:00:00'::interval))
);

ALTER TABLE ONLY crawler.rate_limit_rule
    ADD CONSTRAINT rate_limit_rule_pkey PRIMARY KEY (host);
