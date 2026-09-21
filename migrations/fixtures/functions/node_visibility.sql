-- A node is written for the same audience as the node it sits under, so a new
-- child takes its parent's visibility rather than the column default. The editor
-- never sends the column; it is set here or by an explicit change.
--
-- The parent may not be there yet: the key is deferred, so a whole new subtree can
-- be saved in one statement in any order. save_outline settles those once the
-- payload is in.
create or replace function app_private.tg_document_node__inherit_visibility()
  returns trigger
  language plpgsql
  security definer
  set search_path to public, pg_temp
as $$
declare
  inherited node_visibility;
begin
  select parent.visibility into inherited
  from document_node parent where parent.id = new.parent_id;
  if inherited is not null then
    new.visibility := inherited;
  end if;
  return new;
end;
$$;

-- Changing a node's audience changes its subtree's: that is what makes the
-- policies able to read one column instead of walking ancestors on every row.
-- One statement covers the whole subtree; the depth check then stops each
-- descendant's own trigger from spreading what has already been spread. It has to
-- live here rather than in the trigger's WHEN clause, where pg_trigger_depth() is
-- still 0.
create or replace function app_private.tg_document_node__spread_visibility()
  returns trigger
  language plpgsql
  security definer
  set search_path to public, pg_temp
as $$
begin
  if pg_trigger_depth() > 1 then
    return null;
  end if;

  update document_node node set visibility = new.visibility
  from document_subtree(new.id) descendant
  where node.id = descendant.id
    and node.id <> new.id
    and node.visibility <> new.visibility;
  return null;
end;
$$;

-- Inheritance happens when a node arrives and when it moves, not on every save:
-- `update of parent_id` fires whenever the column is assigned, so a save that
-- rewrites an unchanged parent would otherwise undo a node's own audience. The two
-- cases are separate triggers because a WHEN clause cannot read OLD on an insert.
drop trigger if exists _200_inherit_visibility on document_node;
drop trigger if exists _200_inherit_visibility_insert on document_node;
create trigger _200_inherit_visibility_insert before insert on document_node
  for each row when (new.parent_id is not null)
  execute function app_private.tg_document_node__inherit_visibility();

drop trigger if exists _200_inherit_visibility_move on document_node;
create trigger _200_inherit_visibility_move before update of parent_id on document_node
  for each row when (new.parent_id is not null and old.parent_id is distinct from new.parent_id)
  execute function app_private.tg_document_node__inherit_visibility();

drop trigger if exists _200_spread_visibility on document_node;
create trigger _200_spread_visibility after update of visibility on document_node
  for each row when (old.visibility is distinct from new.visibility)
  execute function app_private.tg_document_node__spread_visibility();
