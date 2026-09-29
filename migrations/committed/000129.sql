--! Previous: sha1:85b7bda0687a2fc952bf138bf6cee654e3200610
--! Hash: sha1:7a45ec3dd5bbe07131d6b4f47cedd0c8e2af7729

--! split: 1-current.sql
do $$
begin
  if exists (select 1 from pg_class where oid = to_regclass('public.tenant_location') and relkind = 'r') then
    alter table tenant_location rename to location;
  end if;
end;
$$;

do $$
begin
  if exists (select 1 from pg_class where oid = to_regclass('public.tenant_location_image') and relkind = 'r') then
    alter table tenant_location_image rename to location_image;
  end if;
end;
$$;

create or replace view tenant_location with (security_invoker = true)
  as select * from location;

create or replace view tenant_location_image with (security_invoker = true)
  as select * from location_image;

comment on table location is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection -queryField:resource:connection';

comment on view tenant_location is '@primaryKey id
@unique tenant_id,id
@foreignKey (tenant_id) references tenant(id)
@foreignKey (tenant_id,cover_image_id) references file(tenant_id,id)|@fieldName coverImage|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@simpleCollections only
@behavior -insert -update -delete -query:resource:list -query:resource:connection -queryField:resource:connection';

comment on column tenant_location.name is '@notNull';
comment on column tenant_location.description is '@notNull';
comment on column tenant_location.tenant_id is '@notNull';
comment on column tenant_location.created_at is '@notNull';
comment on column tenant_location.updated_at is '@notNull';
comment on column tenant_location.show_in_lists is '@notNull';
comment on column tenant_location.ordering is '@notNull';

comment on table access_event is '@omit create,update,delete
@simpleCollections only
@foreignKey (tenant_id,location_id) references tenant_location(tenant_id,id)|@fieldName tenantLocation|@foreignFieldName accessEvents';
comment on table event_instance is '@omit create
@simpleCollections only
@foreignKey (tenant_id,location_id) references tenant_location(tenant_id,id)|@fieldName location|@foreignFieldName eventInstances';
comment on table location_image is '@omit create,update,delete
@simpleCollections only';

comment on view tenant_location_image is '@primaryKey tenant_id,location_id,file_id
@foreignKey (tenant_id,location_id) references tenant_location(tenant_id,id)|@fieldName location|@foreignFieldName images
@foreignKey (tenant_id,file_id) references file(tenant_id,id)|@fieldName file|@foreignFieldName locationImages
@omit create,update,delete
@simpleCollections only
@behavior -insert -update -delete';

comment on column tenant_location_image.tenant_id is '@notNull';
comment on column tenant_location_image.location_id is '@notNull';
comment on column tenant_location_image.file_id is '@notNull';

-- Expose Location separately while legacy readers continue through the compatibility view.
comment on constraint access_event_location_fkey on access_event is '@fieldName locationRecord
@foreignFieldName accessEvents';
comment on constraint event_instance_location_fkey on event_instance is '@fieldName locationRecord
@foreignFieldName eventInstances';
comment on constraint tenant_location_image_location_fk on location_image is '@fieldName location
@foreignFieldName images';
comment on constraint tenant_location_image_file_fk on location_image is '@fieldName file
@foreignFieldName locationImageRecords';

revoke all on tenant_location from public, anonymous;
grant select on tenant_location to anonymous;
revoke all on tenant_location_image from public, anonymous;
grant select on tenant_location_image to anonymous;

--! Included functions/upsert_location.sql
drop function if exists upsert_location;
drop type if exists location_details_input;

create type location_details_input as (
  id bigint,
  name text,
  long_name text,
  description text,
  address address_domain,
  show_in_lists boolean,
  ordering integer,
  latitude double precision,
  longitude double precision
);

create or replace function upsert_location(
  details location_details_input,
  image_ids bigint[] default null,
  cover_image_id bigint default null
)
returns location
language plpgsql
as $$
declare
  result location;
begin
  if details.id is null then
    insert into location (
      name,
      long_name,
      description,
      address,
      show_in_lists,
      ordering,
      latitude,
      longitude,
      cover_image_id
    )
    values (
      details.name,
      nullif(nullif(btrim(details.long_name), ''), details.name),
      coalesce(details.description, ''),
      details.address,
      coalesce(details.show_in_lists, false),
      coalesce(details.ordering, 1),
      details.latitude,
      details.longitude,
      cover_image_id
    )
    returning * into result;
  else
    update location
    set name = details.name,
        long_name = nullif(nullif(btrim(details.long_name), ''), details.name),
        description = coalesce(details.description, ''),
        address = details.address,
        show_in_lists = coalesce(details.show_in_lists, location.show_in_lists),
        ordering = coalesce(details.ordering, location.ordering),
        latitude = details.latitude,
        longitude = details.longitude,
        cover_image_id = upsert_location.cover_image_id
    where id = details.id
    returning * into result;

    if not found then
      raise exception 'Location with id % not found', details.id;
    end if;
  end if;

  if image_ids is not null then
    select coalesce(array_agg(id), '{}'::bigint[])
    into image_ids
    from file
    where id = any(image_ids)
      and tenant_id = result.tenant_id
      and uploaded_at is not null
      and content_type like 'image/%';

    delete from location_image image
    where image.tenant_id = result.tenant_id
      and image.location_id = result.id
      and image.file_id <> all(image_ids);

    insert into location_image (tenant_id, location_id, file_id)
    select result.tenant_id, result.id, file_id
    from unnest(image_ids) input(file_id)
    on conflict do nothing;
  end if;

  return result;
end;
$$;

revoke all on function upsert_location from anonymous;
grant all on function upsert_location to administrator;
--! EndIncluded functions/upsert_location.sql
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

grant execute on function app_private.visible_file_ids() to anonymous;
--! EndIncluded functions/visible_file_ids.sql
