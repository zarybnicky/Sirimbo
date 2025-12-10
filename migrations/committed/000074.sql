--! Previous: sha1:0d3b9d30051eecd578d3179a2292a39aa89fc576
--! Hash: sha1:78e04d1309ab405810af3fe329741a9fe7151430

--! split: 1-current.sql
CREATE or replace FUNCTION app_private.tg_event_instance__delete_payment_on_cancellation() RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER AS $$
declare
  payment_id bigint;
begin
  delete from payment where event_instance_id = OLD.id;

  if not new.is_cancelled then
    select (create_event_instance_payment(event_instance)).id into payment_id
    from event_instance join event on event.id=event_id
    where type='lesson'
      and event_instance.id = NEW.id
      and not event_instance.is_cancelled
      and event_instance.since < now()
      and payment_type = 'after_instance'
      and not exists (
        select * from payment where event_instance_id=event_instance.id
      );

    update payment set status ='unpaid' where id = payment_id;
    perform resolve_payment_with_credit(payment.*) from payment where id = payment_id;
  end if;

  return OLD;
end;
$$;

select verify_function('app_private.tg_event_instance__delete_payment_on_cancellation', 'event_instance');

DROP TRIGGER IF EXISTS _500_delete_on_cancellation on public.event_instance;

CREATE TRIGGER _500_delete_on_cancellation
  AFTER UPDATE ON public.event_instance
  FOR EACH ROW
  EXECUTE FUNCTION app_private.tg_event_instance__delete_payment_on_cancellation();

--! split: 3-crawler.sql
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
CREATE INDEX IF NOT EXISTS frontier_federation_kind_idx ON crawler.frontier (federation, kind);
CREATE INDEX IF NOT EXISTS frontier_federation_kind_next_fetch_at_idx ON crawler.frontier (federation, kind, next_fetch_at)
  WHERE fetch_status = 'pending';

CREATE TABLE IF NOT EXISTS crawler.incremental_ranges (
  federation   text NOT NULL,
  kind         text NOT NULL,        -- 'member_id'
  last_known   integer NOT NULL DEFAULT 0,
  last_checked integer NOT NULL DEFAULT 0,
  PRIMARY KEY (federation, kind)
);

INSERT INTO crawler.incremental_ranges (federation, kind, last_known, last_checked)
VALUES ('csts', 'member_id', 0, 0)
ON CONFLICT (federation, kind) DO NOTHING;


CREATE TABLE IF NOT EXISTS crawler.html_response_cache (
  content_hash text GENERATED ALWAYS AS (encode(digest(content, 'sha256'), 'hex')) STORED,
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
  content_hash text GENERATED ALWAYS AS (encode(digest(content::text, 'sha256'), 'hex')) STORED,
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
      SELECT GREATEST(r.next_available_at, COALESCE(MAX(run_at), now()) + r.spacing)
      FROM graphile_worker.jobs
      WHERE key LIKE 'fetch:' || r.host || ':%'
    );
    RETURN;
  END IF;
END;
$$;
select verify_function('crawler.reserve_request');

--! split: 4-crawler-migrate.sql
insert into crawler.frontier
 (federation, kind, key, discovered_at, last_fetched_at, next_fetch_at, fetch_status, process_status)
select
  'csts',
  'member',
  substring(url from '[0-9]+$'),
  checked_at,
  checked_at,
  now() + interval '1 day' + random() * interval '1 hour',
  'ok',
  'pending'
from csts.ingest
where payload->'collection'->0 is not null
ON CONFLICT (federation, kind, key) DO NOTHING;

INSERT INTO crawler.json_response_cache (content)
SELECT DISTINCT payload
FROM csts.ingest
WHERE payload->'collection'->0 is not null
ON CONFLICT (content_hash) DO NOTHING;

WITH src AS (
  SELECT
    i.url,
    i.payload::jsonb AS content,
    encode(digest((i.payload)::TEXT, 'sha256'), 'hex') AS content_hash,
    substring(i.url from '[0-9]+$') AS key
  FROM csts.ingest i
  WHERE payload->'collection'->0 is not null
)
INSERT INTO crawler.json_response (frontier_id, url, http_status, content_hash)
SELECT
  f.id AS frontier_id,
  s.url,
  200 AS http_status,
  s.content_hash
FROM src s
JOIN crawler.frontier f
  ON f.federation = 'csts' AND f.kind = 'member' AND f.key = s.key
WHERE NOT EXISTS (
  select 1 FROM crawler.json_response ex WHERE ex.frontier_id = f.id AND ex.content_hash = s.content_hash
);

with max_idt as (
  select idt from
  (select substring(url from '[0-9]+$')::integer as idt
  from csts.ingest
  where payload->'collection'->0 is not null) i
  order by case
    when idt between 18000000 and 18092599 then 1
    when idt between 10600000 and 17999999 then 2
    when idt between 18095000 and 19999000 then 3
    else 0
  end desc,
  idt desc
  limit 1
)
update crawler.incremental_ranges
set last_known = (select coalesce(idt, 0) from max_idt),
    last_checked = (select coalesce(idt, 0) from max_idt)
where federation = 'csts' and kind = 'member_id' and exists (select idt from max_idt);
