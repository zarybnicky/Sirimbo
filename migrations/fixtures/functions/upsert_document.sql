drop function if exists upsert_document;
drop type if exists document_input;
drop type if exists document_node_input;

create type document_input as (
  id bigint,
  title text,
  event_instance_id bigint,
  event_series_id bigint,
  cohort_id bigint,
  show_to_members boolean
);

create type document_node_input as (
  id uuid,
  parent_id uuid,
  ordering numeric,
  content jsonb
);

create function upsert_document(doc document_input, nodes document_node_input[])
  returns document
  language plpgsql
as $$
declare
  saved document;
begin
  if doc.id is null then
    insert into document (title, event_instance_id, event_series_id, cohort_id, show_to_members)
    values (
      doc.title,
      doc.event_instance_id,
      doc.event_series_id,
      doc.cohort_id,
      coalesce(doc.show_to_members, false)
    )
    returning * into saved;
  else
    update document set
      title = doc.title,
      event_instance_id = doc.event_instance_id,
      event_series_id = doc.event_series_id,
      cohort_id = doc.cohort_id,
      show_to_members = coalesce(doc.show_to_members, document.show_to_members)
    where id = doc.id
    returning * into saved;

    if not found then
      raise exception 'DOCUMENT_NOT_FOUND' using errcode = '42501';
    end if;
  end if;

  -- Parents are attached in a second pass, so the payload does not have to
  -- arrive in any particular order to satisfy the self-referencing key. Content
  -- is whatever block the editor stores, so there is no default worth inventing;
  -- a null is a not-null violation naming the column.
  insert into document_node (id, tenant_id, document_id, parent_id, ordering, content)
  select
    input.id,
    saved.tenant_id,
    saved.id,
    null,
    coalesce(input.ordering, 1),
    input.content
  from unnest(nodes) input
  on conflict (id) do update set
    ordering = excluded.ordering,
    content = excluded.content;

  update document_node node set parent_id = input.parent_id
  from unnest(nodes) input
  where node.id = input.id
    and node.parent_id is distinct from input.parent_id;

  -- Anything the client no longer sends is gone; children of a deleted node
  -- that moved elsewhere have already been re-parented above.
  delete from document_node
  where document_id = saved.id
    and not exists (select 1 from unnest(nodes) input where input.id = document_node.id);

  return saved;
end;
$$;

comment on function upsert_document(document_input, document_node_input[]) is
  'Saves a document and the whole of its outline. Tags follow from node content.';

grant execute on function upsert_document(document_input, document_node_input[]) to anonymous;
