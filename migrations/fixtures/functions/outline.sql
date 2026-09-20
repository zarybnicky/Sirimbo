drop function if exists save_outline;
drop function if exists document_mentions;
drop function if exists add_outline_node;
drop type if exists outline_node_input;

-- Read-only, so it is a query rather than a mutation. A tenant that has never
-- saved has no row yet, and the first save creates one.
create or replace function tenant_document() returns document
  language sql stable
as $$
  select * from document where tenant_id = current_tenant_id();
$$;

comment on function tenant_document() is 'The tenant''s outline, once it exists.';

-- A tree that does not exist yet has no version to disagree with, so it is
-- created holding whatever version the caller saved against.
create or replace function app_private.ensure_tenant_document(base_version bigint)
  returns document
  language plpgsql
as $$
declare
  found_document document;
begin
  select * into found_document from document where tenant_id = current_tenant_id();
  if found then
    return found_document;
  end if;

  insert into document (title, version) values ('Outline', base_version)
  on conflict (tenant_id) do update set title = document.title
  returning * into found_document;

  return found_document;
end;
$$;

-- A node together with everything under it. That is the unit a tag marks, so it
-- is also what a mention of the tagged entity shows.
create or replace function document_node_subtree(node document_node)
  returns setof document_node
  language sql stable
as $$
  with recursive tree as (
    select self.* from document_node self where self.id = node.id
    union all
    select child.* from document_node child join tree on child.parent_id = tree.id
  )
  select * from tree;
$$;

comment on function document_node_subtree(document_node) is '@simpleCollections only';

-- The ancestors of a node, outermost first, for a breadcrumb. The node itself is
-- not included.
create or replace function document_node_ancestors(node document_node)
  returns setof document_node
  language sql stable
as $$
  with recursive up as (
    select node.parent_id as id, 1 as depth
    union all
    select parent.parent_id, up.depth + 1
    from document_node parent join up on parent.id = up.id
    where parent.parent_id is not null
  )
  select ancestor.*
  from up join document_node ancestor on ancestor.id = up.id
  order by up.depth desc;
$$;

comment on function document_node_ancestors(document_node) is '@simpleCollections only';

-- Loads one subtree by id. A null root means every root of the tenant's tree,
-- which is how the whole outline is loaded.
create or replace function document_subtree(root uuid default null)
  returns setof document_node
  language sql stable
as $$
  select descendant.*
  from document_node node
  cross join lateral document_node_subtree(node) descendant
  where case when root is null then node.parent_id is null else node.id = root end;
$$;

comment on function document_subtree(uuid) is '@simpleCollections only';

-- The same breadcrumb as the computed column, for a node the caller only has an
-- id for.
create or replace function document_node_path(node uuid)
  returns setof document_node
  language sql stable
as $$
  select ancestor.*
  from document_node self
  cross join lateral document_node_ancestors(self) ancestor
  where self.id = node;
$$;

comment on function document_node_path(uuid) is '@simpleCollections only';

create type outline_node_input as (
  id uuid,
  parent_id uuid,
  ordering numeric,
  content jsonb
);

-- Saves one subtree. `root` null saves the whole tree. The delete is scoped to
-- what was under `root` when the save began, so editing a zoomed-in subtree
-- cannot touch anything outside it.
--
-- Folding must not narrow what is sent: a collapsed subtree is still part of the
-- payload, it is only left unrendered. Anything within scope and absent from the
-- payload is taken to be deleted.
create function save_outline(root uuid, base_version bigint, nodes outline_node_input[])
  returns document
  language plpgsql
as $$
declare
  saved document;
  scope_ids uuid[];
begin
  saved := app_private.ensure_tenant_document(base_version);

  if saved.version <> base_version then
    raise exception 'OUTLINE_STALE' using
      errcode = '40001',
      detail = format('document is at version %s, save was based on %s',
                      saved.version, base_version);
  end if;

  -- Captured before any write, so a node moved within the tree is still judged
  -- against where it started.
  select coalesce(array_agg(id), '{}'::uuid[]) into scope_ids
  from document_subtree(root);

  -- Parents are attached in a second pass, so the payload does not have to
  -- arrive in any particular order to satisfy the self-referencing key.
  insert into document_node (id, tenant_id, document_id, parent_id, ordering, content)
  select input.id, saved.tenant_id, saved.id, null, coalesce(input.ordering, 1), input.content
  from unnest(nodes) input
  on conflict (id) do update set
    ordering = excluded.ordering,
    content = excluded.content;

  -- The zoomed root keeps its place in the tree: the editor only ever saw the
  -- subtree, so the payload calls it top level.
  update document_node node set parent_id = input.parent_id
  from unnest(nodes) input
  where node.id = input.id
    and node.id is distinct from root
    and node.parent_id is distinct from input.parent_id;

  delete from document_node
  where document_id = saved.id
    and id = any (scope_ids)
    and not exists (select 1 from unnest(nodes) input where input.id = document_node.id);

  update document set version = version + 1 where id = saved.id returning * into saved;
  return saved;
end;
$$;

comment on function save_outline(uuid, bigint, outline_node_input[]) is
  'Saves one subtree of the tenant outline, rejecting a save based on a stale version.';

-- Appending a single node does not need the caller to hold the tree, so a quick
-- add from a page that only shows mentions never has to load the outline. It
-- still bumps the version: an editor that has the tree open must refetch rather
-- than delete a node it never saw.
create function add_outline_node(parent uuid, content jsonb)
  returns document_node
  language plpgsql
as $$
declare
  target document;
  added document_node;
begin
  target := app_private.ensure_tenant_document(1);

  insert into document_node (document_id, parent_id, ordering, content)
  select target.id, parent, coalesce(max(sibling.ordering), 0) + 1, add_outline_node.content
  from document_node sibling
  where sibling.document_id = target.id
    and sibling.parent_id is not distinct from parent
  returning * into added;

  update document set version = version + 1 where id = target.id;
  return added;
end;
$$;

comment on function add_outline_node(uuid, jsonb) is
  'Appends one node under `parent`, or at the top level when it is null.';

grant execute on function app_private.ensure_tenant_document(bigint) to anonymous;
grant execute on function tenant_document() to anonymous;
grant execute on function document_node_subtree(document_node) to anonymous;
grant execute on function document_node_ancestors(document_node) to anonymous;
grant execute on function document_subtree(uuid) to anonymous;
grant execute on function document_node_path(uuid) to anonymous;
grant execute on function save_outline(uuid, bigint, outline_node_input[]) to anonymous;
grant execute on function add_outline_node(uuid, jsonb) to anonymous;
