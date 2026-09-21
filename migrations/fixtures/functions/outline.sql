drop function if exists save_outline;
drop function if exists document_mentions;
drop function if exists add_outline_node;
drop function if exists app_private.ensure_tenant_document;
drop type if exists outline_node_input;

-- Read-only, so it is a query rather than a mutation. A tenant that has never
-- saved has no row yet, and the first save creates one. Nothing hangs off it but
-- the nodes: concurrency is per node, so there is no document-wide version to
-- agree on.
create or replace function tenant_document() returns document
  language sql stable
as $$
  select * from document where tenant_id = current_tenant_id();
$$;

comment on function tenant_document() is 'The tenant''s outline, once it exists.';

create or replace function app_private.ensure_tenant_document()
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

  insert into document (title) values ('Outline')
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
  content jsonb,
  version bigint
);

-- Saves the nodes the editor is holding. Each carries the version it was loaded
-- at, so two people working on different parts of the outline never collide, and
-- a node with no version is one the editor has just created. Every node names its
-- own parent, the zoomed-in root included -- the caller knows where its root
-- hangs, and `root` is here only to keep a deletion inside that subtree.
--
-- Absence means nothing: a node the editor never rendered -- a collapsed subtree,
-- or anything outside a zoomed-in root -- is simply not in `nodes`. Deletions are
-- named in `deleted`, which is what lets folding drop blocks from the editor
-- entirely rather than hiding them.
create function save_outline(root uuid, nodes outline_node_input[], deleted uuid[] default '{}')
  returns setof document_node
  language plpgsql
as $$
declare
  saved document;
  stale uuid[];
  fresh uuid[];
  scope_ids uuid[];
begin
  saved := app_private.ensure_tenant_document();

  select coalesce(array_agg(input.id), '{}'::uuid[]) into stale
  from unnest(nodes) input
  join document_node node on node.id = input.id
  where input.version is not null and node.version <> input.version;

  if cardinality(stale) > 0 then
    raise exception 'OUTLINE_STALE' using
      errcode = '40001',
      detail = format('%s node(s) moved on since they were loaded: %s',
                      cardinality(stale), array_to_string(stale, ', '));
  end if;

  -- Which of these are new has to be settled before they are written, and it is
  -- only those whose audience is still open to inheritance below.
  select coalesce(array_agg(input.id), '{}'::uuid[]) into fresh
  from unnest(nodes) input
  where not exists (select 1 from document_node node where node.id = input.id);

  -- One statement: the parent key is deferred, so children may arrive before the
  -- parents they hang from.
  insert into document_node (id, tenant_id, document_id, parent_id, ordering, content)
  select input.id, saved.tenant_id, saved.id, input.parent_id,
         coalesce(input.ordering, 1), input.content
  from unnest(nodes) input
  on conflict (id) do update set
    parent_id = excluded.parent_id,
    ordering = excluded.ordering,
    content = excluded.content,
    -- An editor that saves without having changed anything should not invalidate
    -- everyone else's copy.
    version = case
      when (document_node.parent_id, document_node.ordering, document_node.content)
           is distinct from (excluded.parent_id, excluded.ordering, excluded.content)
      then document_node.version + 1
      else document_node.version
    end;

  -- A new child may have been written before its parent, in which case it could
  -- not inherit yet. Only the new ones: a node that already existed keeps the
  -- audience it was given, which is the whole point of the column.
  update document_node child set visibility = parent.visibility
  from document_node parent
  where child.parent_id = parent.id
    and child.visibility <> parent.visibility
    and child.id = any (fresh);

  -- Scoped all the same, so a zoomed-in editor cannot delete outside its subtree.
  select coalesce(array_agg(id), '{}'::uuid[]) into scope_ids
  from document_subtree(root);

  delete from document_node
  where document_id = saved.id
    and id = any (deleted)
    and id = any (scope_ids);

  return query
    select node.* from document_node node
    join unnest(nodes) input on input.id = node.id;
end;
$$;

comment on function save_outline(uuid, outline_node_input[], uuid[]) is
  'Saves the nodes an outline editor holds, rejecting any that moved on since they were loaded.';

-- Appending a single node does not need the caller to hold the tree, so a quick
-- add from a page that only shows mentions never has to load the outline.
create function add_outline_node(parent uuid, content jsonb)
  returns document_node
  language plpgsql
as $$
declare
  target document;
  added document_node;
begin
  target := app_private.ensure_tenant_document();

  insert into document_node (document_id, parent_id, ordering, content)
  select target.id, parent, coalesce(max(sibling.ordering), 0) + 1, add_outline_node.content
  from document_node sibling
  where sibling.document_id = target.id
    and sibling.parent_id is not distinct from parent
  returning * into added;

  return added;
end;
$$;

comment on function add_outline_node(uuid, jsonb) is
  'Appends one node under `parent`, or at the top level when it is null.';

grant execute on function app_private.ensure_tenant_document() to anonymous;
grant execute on function tenant_document() to anonymous;
grant execute on function document_node_subtree(document_node) to anonymous;
grant execute on function document_node_ancestors(document_node) to anonymous;
grant execute on function document_subtree(uuid) to anonymous;
grant execute on function document_node_path(uuid) to anonymous;
grant execute on function save_outline(uuid, outline_node_input[], uuid[]) to anonymous;
grant execute on function add_outline_node(uuid, jsonb) to anonymous;
