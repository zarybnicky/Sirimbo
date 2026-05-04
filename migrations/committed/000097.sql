--! Previous: sha1:74faa7fd530166ad039377826ac50620f1d49fa6
--! Hash: sha1:c33857632e79aae14292a17d03319c0605ab6fcc

--! split: 1-current.sql
INSERT INTO crawler.json_response_cache (content)
SELECT to_jsonb(hc.content)
FROM crawler.html_response_cache AS hc
ON CONFLICT (content_hash) DO NOTHING;

INSERT INTO crawler.json_response (frontier_id, url, http_status, error, content_hash, fetched_at)
SELECT
  hr.frontier_id,
  hr.url,
  hr.http_status,
  hr.error,
  CASE
    WHEN hr.content_hash IS NOT NULL
    THEN encode(digest(to_jsonb(hc.content)::text, 'sha256'), 'hex')
  END AS content_hash,
  hr.fetched_at
FROM crawler.html_response hr
LEFT JOIN crawler.html_response_cache hc ON hr.content_hash = hc.content_hash
WHERE NOT EXISTS (SELECT 1 FROM crawler.json_response jr WHERE jr.frontier_id = hr.frontier_id AND jr.fetched_at = hr.fetched_at);

alter type crawler.fetch_status ADD VALUE IF NOT EXISTS 'transient';

ALTER TABLE crawler.frontier
  ADD COLUMN IF NOT EXISTS last_process_error text;

ALTER TABLE crawler.frontier
  ADD COLUMN IF NOT EXISTS last_process_error_at timestamptz;

UPDATE crawler.frontier
SET last_process_error_at = coalesce(last_fetched_at, discovered_at)
WHERE process_status = 'error'
  AND last_process_error_at IS NULL;
