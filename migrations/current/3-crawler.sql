drop schema if exists crawler cascade;

create schema IF NOT EXISTS crawler;

DO $$
  BEGIN
    IF to_regtype('crawler.fetch_status') IS NULL THEN
      CREATE TYPE crawler.fetch_status AS ENUM (
        'pending',    -- needs (re)fetch
        'ok',         -- fetched successfully
        'gone',       -- confirmed 404/empty
        'error'       -- fetch repeatedly failed
      );
    END IF;
  END;
$$;

DO $$
  BEGIN
    IF to_regtype('crawler.process_status') IS NULL THEN
      CREATE TYPE crawler.process_status AS ENUM (
        'pending',    -- needs processing
        'ok',         -- processed successfully
        'error'       -- processing failed
      );
    END IF;
  END;
$$;

CREATE TABLE IF NOT EXISTS crawler.frontier (
  id              bigserial PRIMARY KEY,
  federation      text NOT NULL,
  kind            text NOT NULL,       -- 'event_index', 'event_page', 'member', 'results', ...
  key             text NOT NULL,       -- URL or synthetic ID like 'member:12345'
  discovered_at   timestamptz NOT NULL DEFAULT now(),
  last_fetched_at timestamptz,
  fetch_status    crawler.fetch_status NOT NULL DEFAULT 'pending',
  process_status  crawler.process_status NOT NULL DEFAULT 'pending',
  error_count     integer NOT NULL DEFAULT 0,
  next_fetch_at   timestamptz,
  meta            jsonb NOT NULL DEFAULT '{}'::jsonb,  -- depth, referrer, date range, etc.
  UNIQUE (federation, kind, key)
);
CREATE INDEX ON crawler.frontier (federation, kind);
CREATE INDEX ON crawler.frontier (federation, kind, next_fetch_at)
  WHERE fetch_status = 'pending';

CREATE TABLE IF NOT EXISTS crawler.incremental_ranges (
  federation   text NOT NULL,
  kind         text NOT NULL,        -- 'member_id'
  last_known   integer NOT NULL DEFAULT 0,
  last_checked integer NOT NULL DEFAULT 0,
  PRIMARY KEY (federation, kind)
);

INSERT INTO crawler.incremental_ranges (federation, kind)
VALUES ('csts', 'member_id')
ON CONFLICT (federation, kind) DO NOTHING;


CREATE TABLE IF NOT EXISTS crawler.html_response_cache (
  content_hash text GENERATED ALWAYS AS (encode(sha256(content::TEXT::BYTEA), 'hex')) STORED,
  content      text,
  PRIMARY KEY (content_hash)
);

CREATE TABLE IF NOT EXISTS crawler.html_response (
  id           bigserial PRIMARY KEY,
  frontier_id  bigint NOT NULL REFERENCES crawler.frontier(id) ON DELETE CASCADE,
  url          text NOT NULL,
  fetched_at   timestamptz NOT NULL DEFAULT now(),
  http_status  integer,
  error        text,
  content_hash text REFERENCES crawler.html_response_cache (content_hash)
);

CREATE TABLE IF NOT EXISTS crawler.json_response_cache (
  content_hash text GENERATED ALWAYS AS (encode(sha256(content::TEXT::BYTEA), 'hex')) STORED,
  content      jsonb,
  PRIMARY KEY (content_hash)
);

CREATE TABLE IF NOT EXISTS crawler.json_response (
  id           bigserial PRIMARY KEY,
  frontier_id  bigint NOT NULL REFERENCES crawler.frontier(id) ON DELETE CASCADE,
  url          text NOT NULL,
  fetched_at   timestamptz NOT NULL DEFAULT now(),
  http_status  integer,
  error        text,
  content_hash text REFERENCES crawler.json_response_cache (content_hash)
);

CREATE TABLE IF NOT EXISTS crawler.rate_limit_rule (
  host              text PRIMARY KEY,        -- e.g. 'example.com'
  max_requests      integer NOT NULL,        -- e.g. 5
  per_interval      interval NOT NULL,       -- e.g. '1 second', '1 minute'
  spacing           interval not null GENERATED ALWAYS AS ((per_interval / max_requests) + interval '20 milliseconds') STORED,
  next_available_at timestamptz NOT NULL DEFAULT '1970-01-01',
  CHECK (max_requests > 0),
  CHECK (per_interval > interval '0 seconds')
);

INSERT INTO crawler.rate_limit_rule (host, max_requests, per_interval)
VALUES ('www.csts.cz', 60, interval '1 minute'),
       ('services.worlddancesport.org', 60, interval '1 minute')
ON CONFLICT (host)
  DO UPDATE SET max_requests = EXCLUDED.max_requests, per_interval = EXCLUDED.per_interval;

CREATE OR REPLACE FUNCTION crawler.reserve_request(
  in_host       text,
  OUT granted    boolean,
  OUT allowed_at timestamptz
)
  LANGUAGE plpgsql AS $$
DECLARE
  r crawler.rate_limit_rule;
BEGIN
  SELECT *
  INTO r
  FROM crawler.rate_limit_rule
  WHERE host = in_host
  FOR UPDATE;

  IF NOT FOUND THEN
    granted := true;
    allowed_at := now();
    RETURN;
  END IF;

  IF r.next_available_at <= now() THEN
    UPDATE crawler.rate_limit_rule
       SET next_available_at = now() + r.spacing
     WHERE host = in_host;

    granted := true;
    allowed_at := now();
    RETURN;
  ELSE
    granted := false;
    allowed_at := (
      SELECT GREATEST(r.next_available_at, COALESCE(MAX(run_at), now())) + r.spacing
      FROM graphile_worker.jobs
      WHERE key LIKE 'fetch:' || r.host || ':%'
    );
    RETURN;
  END IF;
END;
$$;
select verify_function('crawler.reserve_request');
