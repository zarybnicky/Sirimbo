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

insert into crawler.json_response
  (frontier_id, url, http_status, content)
select
  (select id from crawler.frontier where federation = 'csts' and kind = 'member' and key = substring(url from '[0-9]+$')),
  url,
  200,
  payload
from csts.ingest
where payload <> '{"collection": []}';

with max_idt as (
  select substring(url from '[0-9]+$')::integer as idt
  from csts.ingest
  where payload <> '{"collection": []}'
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
