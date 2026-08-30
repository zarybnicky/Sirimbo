CREATE FUNCTION public.upsert_article(info public.article_type_input, attachments bigint[] DEFAULT NULL::bigint[]) RETURNS public.aktuality
    LANGUAGE plpgsql
    AS $$
declare
  result aktuality;
begin
  if info.id is null then
    insert into aktuality (
      at_jmeno, at_preview, at_text, title_photo_url, is_visible
    ) values (
      info.title,
      coalesce(info.preview, ''),
      coalesce(info.body, ''),
      info.title_photo_url,
      coalesce(info.is_visible, true)
    ) returning * into result;
  else
    select * into result from aktuality where id = info.id;

    if not found then
      raise exception 'Article with id % not found', info.id;
    end if;
  end if;

  if attachments is not null then
    select coalesce(array_agg(id), '{}'::bigint[])
    into attachments
    from file
    where id = any(attachments)
      and tenant_id = result.tenant_id
      and uploaded_at is not null;

    delete from article_attachment
    where aktuality_id = result.id
      and not inline
      and file_id <> all(attachments);

    insert into article_attachment (tenant_id, aktuality_id, file_id, inline)
    select result.tenant_id, result.id, file_id, false
    from unnest(attachments) input(file_id)
    on conflict (tenant_id, aktuality_id, file_id)
    do update set inline = false;
  end if;

  if info.id is not null then
    -- Make sure the update trigger re-populates inline references
    update aktuality set
      at_jmeno = info.title,
      at_preview = coalesce(info.preview, ''),
      at_text = coalesce(info.body, ''),
      title_photo_url = info.title_photo_url,
      is_visible = coalesce(info.is_visible, true)
    where id = info.id
    returning * into result;
  end if;

  return result;
end;
$$;

GRANT ALL ON FUNCTION public.upsert_article(info public.article_type_input, attachments bigint[]) TO anonymous;
