create or replace function app_private.tg_aktuality__sync_attachments()
  returns trigger
  language plpgsql
  as $$
declare
  file_ids bigint[];
begin
  select coalesce(array_agg(distinct file.id), '{}'::bigint[])
  into file_ids
  from regexp_matches(
    concat_ws(' ', new.at_preview, new.at_text, new.title_photo_url),
    '/f/([0-9]+)/',
    'g'
  ) match(parts)
  join file on file.id::text = match.parts[1]
    and file.tenant_id = new.tenant_id
    and file.uploaded_at is not null;

  delete from article_attachment
  where aktuality_id = new.id
    and inline
    and file_id <> all(file_ids);

  insert into article_attachment (tenant_id, aktuality_id, file_id, inline)
  select new.tenant_id, new.id, input.file_id, true
  from unnest(file_ids) input(file_id)
  on conflict (tenant_id, aktuality_id, file_id) do nothing;

  return new;
end;
$$;

comment on function app_private.tg_aktuality__sync_attachments()
  is 'Synchronizes inline file references from article content.';

drop trigger if exists _500_sync_attachments_insert on aktuality;
create trigger _500_sync_attachments_insert
  after insert on aktuality
  for each row execute function app_private.tg_aktuality__sync_attachments();

drop trigger if exists _500_sync_attachments_update on aktuality;
create trigger _500_sync_attachments_update
  before update of at_preview, at_text, title_photo_url on aktuality
  for each row execute function app_private.tg_aktuality__sync_attachments();
