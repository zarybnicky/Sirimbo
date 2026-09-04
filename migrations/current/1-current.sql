drop function if exists public.attachment_directories();
drop function if exists public.attachment_directory(public.attachment);
drop table if exists public.attachment;

update file
set content_type = case
  when name ~* '\.jpe?g$' then 'image/jpeg'
  when name ~* '\.png$' then 'image/png'
  when name ~* '\.pdf$' then 'application/pdf'
end
where content_type is null
  and name ~* '\.(jpe?g|png|pdf)$';

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
