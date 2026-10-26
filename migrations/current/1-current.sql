
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

alter table public.event add column if not exists cover_image_id bigint;
alter table public.event drop constraint if exists event_cover_image_fk;
alter table public.event add constraint event_cover_image_fk
  foreign key (tenant_id, cover_image_id) references public.file (tenant_id, id)
  on delete set null (cover_image_id);
comment on constraint event_cover_image_fk on public.event is '@fieldName coverImage';
create index if not exists event_cover_image_idx on public.event (tenant_id, cover_image_id)
  where cover_image_id is not null;

--!include functions/accessible_event_ids.sql
--!include functions/visible_file_ids.sql
--!include policies/file.sql
--!include functions/set_lesson_request.sql
--!include functions/set_event_registration.sql
