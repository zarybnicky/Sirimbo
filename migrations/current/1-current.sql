
do $$
begin
  if not exists (
    select 1 from pg_attribute
    where attrelid = (select typrelid from pg_type where oid = 'public.competition_participation_record'::regtype)
      and attname = 'final_gain' and not attisdropped
  ) then
    alter type public.competition_participation_record add attribute final_gain numeric(10,3);
  end if;
end;
$$;

grant execute on function federated.class_rank to anonymous;

--!include functions/competition_reports.sql
--!include functions/activity_timeline.sql

--!include policies/person.sql

alter table federated.competitor add column if not exists divisions text[] not null default '{}';
comment on column federated.competitor.divisions is '@behavior -filterBy -orderBy';

update federated.competitor c set divisions = array[cache.content->>'division'], age_group = cache.content->>'age'
from crawler.frontier f
join crawler.json_response r on r.id = f.last_successful_response_id
join crawler.json_response_cache cache on cache.content_hash = r.content_hash
where f.federation = 'wdsf' and f.kind = 'couple' and c.id = 'wdsf:' || (cache.content->>'id')
  and cache.content->>'division' is not null
  and (c.divisions, c.age_group) is distinct from (array[cache.content->>'division'], cache.content->>'age');

--!include views/competition_participation.sql
--!include views/couple_federation_link.sql
--!include functions/competition_suggestions.sql
