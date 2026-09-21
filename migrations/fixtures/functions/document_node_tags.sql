-- document_node_tag is a projection of document_node.content: every tag is an
-- inline node in the document, so the content is the only place one is authored.
create or replace function app_private.tg_document_node__sync_tags()
  returns trigger
  language plpgsql
  security definer
  set search_path to pg_catalog, public, pg_temp
as $$
begin
  delete from document_node_tag where node_id = new.id;

  -- `$.**` reaches the same object by more than one path, hence distinct.
  insert into document_node_tag (
    tenant_id, node_id, person_id, couple_id, cohort_id,
    event_instance_id, event_series_id, competition_id, dance_code, tagged_month, discipline
  )
  select distinct
    new.tenant_id,
    new.id,
    case when kind = 'person' then ref::bigint end,
    case when kind = 'couple' then ref::bigint end,
    case when kind = 'cohort' then ref::bigint end,
    case when kind = 'event' then ref::bigint end,
    case when kind = 'series' then ref::bigint end,
    case when kind = 'competition' then ref::bigint end,
    case when kind = 'dance' then ref end,
    case when kind = 'month' then ref::date end,
    case when kind = 'discipline' then ref::discipline end
  from (
    select tag->'props'->>'kind' as kind, tag->'props'->>'refId' as ref
    from jsonb_path_query(new.content, '$.** ? (@.type == "tag")') tag
  ) mention
  where kind in ('person', 'couple', 'cohort', 'event', 'series', 'competition',
                 'dance', 'month', 'discipline')
    and ref is not null and ref <> ''
  on conflict do nothing;

  return new;
end;
$$;

drop trigger if exists _500_sync_tags_insert on document_node;
create trigger _500_sync_tags_insert
  after insert on document_node
  for each row execute function app_private.tg_document_node__sync_tags();

drop trigger if exists _500_sync_tags_update on document_node;
create trigger _500_sync_tags_update
  after update of content on document_node
  for each row execute function app_private.tg_document_node__sync_tags();

-- The same projection again, for the files a node embeds. It is what makes an
-- uploaded file reachable from the node that uses it -- and an unreferenced
-- upload findable -- without the content having to be parsed at read time.
create or replace function app_private.tg_document_node__sync_files()
  returns trigger
  language plpgsql
  security definer
  set search_path to pg_catalog, public, pg_temp
as $$
begin
  delete from document_node_file where node_id = new.id;

  insert into document_node_file (tenant_id, node_id, file_id)
  select distinct new.tenant_id, new.id, (regexp_match(embedded, '^/f/(\d+)/'))[1]::bigint
  from (
    select url #>> '{}' from jsonb_path_query(new.content, '$.**.url') url
  ) reference(embedded)
  where embedded ~ '^/f/\d+/'
  on conflict do nothing;

  return new;
end;
$$;

drop trigger if exists _500_sync_files_insert on document_node;
create trigger _500_sync_files_insert
  after insert on document_node
  for each row execute function app_private.tg_document_node__sync_files();

drop trigger if exists _500_sync_files_update on document_node;
create trigger _500_sync_files_update
  after update of content on document_node
  for each row execute function app_private.tg_document_node__sync_files();
