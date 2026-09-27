CREATE FUNCTION public.upsert_location(details public.location_details_input, image_ids bigint[] DEFAULT NULL::bigint[], cover_image_id bigint DEFAULT NULL::bigint) RETURNS public.tenant_location
    LANGUAGE plpgsql
    AS $$
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

GRANT ALL ON FUNCTION public.upsert_location(details public.location_details_input, image_ids bigint[], cover_image_id bigint) TO administrator;
