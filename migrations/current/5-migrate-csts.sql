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
set last_known = (select idt from max_idt),
    last_checked = (select idt from max_idt)
where federation = 'csts' and kind = 'member_id';
