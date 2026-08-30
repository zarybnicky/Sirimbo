CREATE FUNCTION app_private.tg_announcement__sync_attachments() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
declare
  file_ids bigint[];
begin
  select coalesce(array_agg(distinct file.id), '{}'::bigint[])
  into file_ids
  from regexp_matches(coalesce(new.body, ''), '/f/([0-9]+)/', 'g') match(parts)
  join file on file.id::text = match.parts[1]
    and file.tenant_id = new.tenant_id
    and file.uploaded_at is not null;

  delete from announcement_attachment
  where announcement_id = new.id
    and inline
    and file_id <> all(file_ids);

  insert into announcement_attachment (tenant_id, announcement_id, file_id, inline)
  select new.tenant_id, new.id, input.file_id, true
  from unnest(file_ids) input(file_id)
  on conflict (tenant_id, announcement_id, file_id) do nothing;

  return new;
end;
$$;

COMMENT ON FUNCTION app_private.tg_announcement__sync_attachments() IS 'Synchronizes inline file references from announcement content.';
