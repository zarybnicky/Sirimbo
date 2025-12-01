insert into crawler.frontier
 (federation, kind, key, last_fetched_at, fetch_status, process_status)
select
  'csts',
  'member',
  substring(url from '[0-9]+$'),
  checked_at,
  'ok',
  'pending'
from csts.ingest
where payload <> '{"collection": []}';

INSERT INTO crawler.json_response_cache (content)
SELECT DISTINCT payload->'collection'->0
FROM csts.ingest
WHERE payload->'collection'->0 is not null
ON CONFLICT (content_hash) DO NOTHING;

WITH src AS (
  SELECT
    i.url,
    i.payload::jsonb AS content,
    encode(sha256((i.payload->'collection'->0)::TEXT::BYTEA), 'hex') AS content_hash,
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
  ON f.federation = 'csts' AND f.kind = 'member' AND f.key = s.key;


with max_idt as (
  select idt from
  (select substring(url from '[0-9]+$')::integer as idt
  from csts.ingest
  where payload <> '{"collection": []}') i
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
where federation = 'csts' and kind = 'member';
