import type { PartialBlock } from '@blocknote/core';

// Structural, so a custom schema's block type is accepted without threading the
// schema generics through every caller.
type AnyBlock = { id: string; children?: AnyBlock[] };

export type OutlineRow = {
  id: string;
  parentId: string | null;
  ordering: number;
  // The block this row stands for, without its children: BlockNote's
  // {id, type, props, content} lines up with the row as stored.
  content: Record<string, unknown>;
};

// Depth first, so a parent always precedes its children and the
// self-referencing foreign key is satisfied by insertion order alone.
export function blocksToRows(blocks: AnyBlock[]): OutlineRow[] {
  const rows: OutlineRow[] = [];

  const walk = (list: AnyBlock[], parentId: string | null) => {
    for (const [index, block] of list.entries()) {
      const { children, ...withoutChildren } = block;
      rows.push({
        id: block.id,
        parentId,
        ordering: index + 1,
        content: withoutChildren as Record<string, unknown>,
      });
      walk(children ?? [], block.id);
    }
  };

  walk(blocks, null);
  return rows;
}

export function rowsToBlocks(rows: OutlineRow[]): PartialBlock[] {
  const byParent = new Map<string | null, OutlineRow[]>();
  for (const row of rows) {
    const siblings = byParent.get(row.parentId);
    if (siblings) {
      siblings.push(row);
    } else {
      byParent.set(row.parentId, [row]);
    }
  }
  for (const siblings of byParent.values()) {
    siblings.sort((a, b) => a.ordering - b.ordering);
  }

  const build = (parentId: string | null): PartialBlock[] =>
    (byParent.get(parentId) ?? []).map(
      (row) => ({ ...row.content, id: row.id, children: build(row.id) }) as PartialBlock,
    );

  const blocks = build(null);
  // BlockNote will not mount on an empty document.
  return blocks.length > 0 ? blocks : [{ type: 'bulletListItem' }];
}
