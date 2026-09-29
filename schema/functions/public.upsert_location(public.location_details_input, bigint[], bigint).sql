CREATE FUNCTION public.upsert_location(details public.location_details_input, image_ids bigint[] DEFAULT NULL::bigint[], cover_image_id bigint DEFAULT NULL::bigint) RETURNS public.location
    LANGUAGE plpgsql
    AS $$
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

GRANT ALL ON FUNCTION public.upsert_location(details public.location_details_input, image_ids bigint[], cover_image_id bigint) TO administrator;
