do $$
begin
  if not exists (
    select 1
    from pg_type type
    join pg_namespace namespace on namespace.oid = type.typnamespace
    where namespace.nspname = 'public' and type.typname = 'location_details_input'
  ) then
    create type location_details_input as (
      id bigint,
      name text,
      description text,
      address address_domain,
      is_public boolean
    );
  end if;
end;
$$;

do $$
begin
  if to_regtype('location_image_input') is not null then
    execute 'drop function if exists upsert_location(location_details_input, location_image_input[])';
    execute 'drop type location_image_input';
  end if;
end;
$$;

create or replace function upsert_location(
  details location_details_input,
  image_ids bigint[] default null,
  cover_image_id bigint default null
)
returns tenant_location
language plpgsql
as $$
declare
  result tenant_location;
begin
  if details.id is null then
    insert into tenant_location (
      name,
      description,
      address,
      is_public,
      cover_image_id
    )
    values (
      details.name,
      coalesce(details.description, ''),
      details.address,
      coalesce(details.is_public, true),
      cover_image_id
    )
    returning * into result;
  else
    update tenant_location
    set name = details.name,
        description = coalesce(details.description, ''),
        address = details.address,
        is_public = coalesce(details.is_public, true),
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

    delete from tenant_location_image image
    where image.tenant_id = result.tenant_id
      and image.location_id = result.id
      and image.file_id <> all(image_ids);

    insert into tenant_location_image (tenant_id, location_id, file_id)
    select result.tenant_id, result.id, file_id
    from unnest(image_ids) input(file_id)
    on conflict do nothing;
  end if;

  return result;
end;
$$;

revoke all on function upsert_location from anonymous;
grant all on function upsert_location to administrator;
