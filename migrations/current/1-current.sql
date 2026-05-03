DROP FUNCTION IF EXISTS public.event_instance_attendance_summary;

DROP FUNCTION IF EXISTS event_instances_for_range(only_type event_type, start_range timestamp with time zone, end_range timestamp with time zone, trainer_ids bigint[], only_mine boolean);
--!include functions/event_instances_for_range.sql

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

DROP INDEX crawler.frontier_federation_kind_next_fetch_at_idx;
CREATE INDEX frontier_federation_kind_next_fetch_at_idx
  ON crawler.frontier (federation, kind, next_fetch_at)
  WHERE fetch_status IN ('pending', 'transient');
