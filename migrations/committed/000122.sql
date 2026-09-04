--! Previous: sha1:756b279bd80b272d38031c8c615dda37621f9d3c
--! Hash: sha1:183a1a8b45e51b743af638f0c23beed8e4740a92

--! split: 1-current.sql
drop function if exists public.attachment_directories();
drop function if exists public.attachment_directory(public.attachment);
drop table if exists public.attachment;

alter table file
  add column if not exists display_name text,
  add column if not exists is_public boolean not null default false;

create index if not exists file_public_idx on file (tenant_id, id) where is_public;

--! Included functions/visible_file_ids.sql
create or replace function app_private.visible_file_ids()
  returns setof bigint
  language sql stable
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  select id
  from file
  where tenant_id = (select current_tenant_id())
    and is_public

  union

  select f.file_id
  from announcement_attachment f
  join app_private.visible_announcement_ids() a(id) on a.id = f.announcement_id
  where f.tenant_id = (select current_tenant_id())

  union

  select f.file_id
  from article_attachment f
  join aktuality a on id = f.aktuality_id and a.tenant_id = f.tenant_id
  where a.tenant_id = (select current_tenant_id())
    and a.is_visible;
$$;

grant execute on function app_private.visible_file_ids() to anonymous;
--! EndIncluded functions/visible_file_ids.sql

update file
set content_type = case
  when name ~* '\.jpe?g$' then 'image/jpeg'
  when name ~* '\.png$' then 'image/png'
  when name ~* '\.pdf$' then 'application/pdf'
end
where content_type is null
  and name ~* '\.(jpe?g|png|pdf)$';

insert into file (
  tenant_id,
  object_key,
  name,
  display_name,
  content_type,
  uploaded_by,
  uploaded_at,
  created_at
)
select
  tenant_id,
  'tkolymp/' || d_path,
  d_filename,
  d_name,
  case lower(reverse(split_part(reverse(d_path), '.', 1)))
    when 'bmp' then 'image/bmp'
    when 'doc' then 'application/msword'
    when 'docx' then 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
    when 'jpg' then 'image/jpeg'
    when 'pdf' then 'application/pdf'
    when 'xls' then 'application/vnd.ms-excel'
  end,
  d_kdo,
  to_timestamp(split_part(split_part(d_path, '/', 2), '.', 1)::double precision),
  to_timestamp(split_part(split_part(d_path, '/', 2), '.', 1)::double precision)
from dokumenty
where d_path <> 'uploads/1686563403.pdf'
on conflict (object_key) do update
set name = excluded.name,
    display_name = excluded.display_name,
    content_type = excluded.content_type,
    uploaded_by = excluded.uploaded_by,
    uploaded_at = excluded.uploaded_at;

update aktuality article
set title_photo_url =
  '/f/' || file.id || '/' ||
  regexp_replace(
    article.title_photo_url,
    '^https://files\.rozpisovnik\.cz/file/rozpisovnik/[^/]+/[0-9]+-',
    ''
  )
from file
where article.tenant_id = file.tenant_id
  and article.title_photo_url =
    'https://files.rozpisovnik.cz/file/rozpisovnik/' ||
    replace(file.object_key, ' ', '%20');

update announcement
set body = regexp_replace(
  body,
  'https://files\.rozpisovnik\.cz/file/rozpisovnik/[^/]+/[0-9]+-',
  '/f/' || input.file_id || '/',
  'g'
)
from (values
  (1141, 6),
  (1168, 32),
  (1178, 36),
  (1192, 39),
  (1203, 45),
  (1228, 53),
  (1245, 57)
) input(id, file_id)
where announcement.id = input.id;

update aktuality
set at_text = regexp_replace(
  at_text,
  'https://files\.rozpisovnik\.cz/file/rozpisovnik/[^/]+/[0-9]+-',
  '/f/' || input.file_id || '/',
  'g'
)
from (values (467, 38)) input(id, file_id)
where aktuality.id = input.id;
