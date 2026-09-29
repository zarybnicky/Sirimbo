CREATE FUNCTION app_private.visible_file_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
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
    and a.is_visible

  union

  select image.file_id
  from location_image image
  join location
    on location.tenant_id = image.tenant_id
    and location.id = image.location_id
  where image.tenant_id = (select current_tenant_id())

  union

  select cover_image_id
  from location
  where tenant_id = (select current_tenant_id())
    and cover_image_id is not null;
$$;

GRANT ALL ON FUNCTION app_private.visible_file_ids() TO anonymous;
