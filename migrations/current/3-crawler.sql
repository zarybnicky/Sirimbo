drop schema if exists crawler cascade;
create schema crawler;
CREATE TYPE crawler.fetch_status AS ENUM (
  'pending',    -- needs (re)fetch
  'ok',         -- fetched successfully
  'gone',       -- confirmed 404/empty
  'error'       -- fetch repeatedly failed
);

CREATE TYPE crawler.process_status AS ENUM (
  'pending',    -- needs processing
  'ok',         -- processed successfully
  'error'       -- processing failed
);

CREATE TABLE crawler.frontier (
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

CREATE TABLE crawler.incremental_ranges (
  federation   text NOT NULL,
  kind         text NOT NULL,        -- 'member_id'
  last_known   integer NOT NULL DEFAULT 0,
  last_checked integer NOT NULL DEFAULT 0,
  PRIMARY KEY (federation, kind)
);

INSERT INTO crawler.incremental_ranges (federation, kind)
VALUES ('csts', 'member_id');


CREATE TABLE crawler.html_response_cache (
  content_hash text GENERATED ALWAYS AS (encode(sha256(content::TEXT::BYTEA), 'hex')) STORED,
  content      jsonb,
  PRIMARY KEY (content_hash)
);

CREATE TABLE crawler.html_response (
  id           bigserial PRIMARY KEY,
  frontier_id  bigint NOT NULL REFERENCES crawler.frontier(id) ON DELETE CASCADE,
  url          text NOT NULL,
  fetched_at   timestamptz NOT NULL DEFAULT now(),
  http_status  integer,
  error        text,
  content_hash text REFERENCES crawler.html_response_cache (content_hash)
);

CREATE TABLE crawler.json_response_cache (
  content_hash text GENERATED ALWAYS AS (encode(sha256(content::TEXT::BYTEA), 'hex')) STORED,
  content      jsonb,
  PRIMARY KEY (content_hash)
);

CREATE TABLE crawler.json_response (
  id           bigserial PRIMARY KEY,
  frontier_id  bigint NOT NULL REFERENCES crawler.frontier(id) ON DELETE CASCADE,
  url          text NOT NULL,
  fetched_at   timestamptz NOT NULL DEFAULT now(),
  http_status  integer,
  error        text,
  content_hash text REFERENCES crawler.json_response_cache (content_hash)
);

CREATE TABLE crawler.rate_limit_rule (
  id             bigserial PRIMARY KEY,
  host           text NOT NULL,           -- e.g. 'example.com'
  path_prefix    text NOT NULL,           -- e.g. '/results', '/api', '/'
  max_requests   integer NOT NULL,        -- e.g. 5
  per_interval   interval NOT NULL,       -- e.g. '1 second', '1 minute'
  enabled        boolean NOT NULL DEFAULT true,
  UNIQUE (host, path_prefix),
  CHECK (max_requests > 0),
  CHECK (per_interval > interval '0 seconds')
);
CREATE INDEX ON crawler.rate_limit_rule (host, path_prefix) WHERE enabled = true;

INSERT INTO crawler.rate_limit_rule (host, path_prefix, max_requests, per_interval)
VALUES ('www.csts.cz', '/api/1', 60, interval '1 minute'),
       ('services.worlddancesport.org', '/api/1', 60, interval '1 minute');

CREATE TABLE crawler.rate_limit_event (
  rule_id   bigint NOT NULL REFERENCES crawler.rate_limit_rule(id) ON DELETE CASCADE,
  occurred_at timestamptz NOT NULL DEFAULT now()
);
CREATE INDEX ON crawler.rate_limit_event (rule_id, occurred_at);

CREATE OR REPLACE FUNCTION crawler.reserve_request(
  in_host       text,
  in_prefixes   text[]
)
RETURNS TABLE (
  granted    boolean,
  allowed_at timestamptz
)
LANGUAGE plpgsql AS $$
DECLARE
  r                crawler.rate_limit_rule;
  window_start     timestamptz;
  req_count        integer;
  oldest_in_window timestamptz;
BEGIN
  -- FOR UPDATE for serialized write access to rate_limit_event table
  SELECT *
  INTO r
  FROM crawler.rate_limit_rule
  WHERE host = in_host
    AND path_prefix = ANY (in_prefixes)
    AND enabled
  ORDER BY length(path_prefix) DESC
  LIMIT 1
  FOR UPDATE;

  IF NOT FOUND THEN
    granted := true;
    allowed_at := now();
    RETURN;
  END IF;

  window_start := now() - r.per_interval;

  DELETE FROM crawler.rate_limit_event
   WHERE rule_id = r.id
     AND occurred_at < window_start;

  SELECT count(*), min(occurred_at)
  INTO req_count, oldest_in_window
  FROM crawler.rate_limit_event
  WHERE rule_id = r.id
    AND occurred_at >= window_start;

  IF req_count < r.max_requests THEN
    -- Enforced: we *count* this request atomically
    INSERT INTO crawler.rate_limit_event(rule_id) VALUES (r.id);

    granted := true;
    allowed_at := now();
    RETURN;
  ELSE
    -- Not allowed now; just compute a hint
    granted := false;
    allowed_at := oldest_in_window + r.per_interval;
    RETURN;
  END IF;
END;
$$;
