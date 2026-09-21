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

// A subtree loaded on its own still names the parent it hangs from, which is not
// in the excerpt; those rows are the ones to start from.
export function rowsToBlocks(rows: OutlineRow[]): PartialBlock[] {
  const present = new Set(rows.map((row) => row.id));
  const byParent = new Map<string | null, OutlineRow[]>();
  for (const row of rows) {
    const parentId =
      row.parentId !== null && present.has(row.parentId) ? row.parentId : null;
    const siblings = byParent.get(parentId);
    if (siblings) {
      siblings.push(row);
    } else {
      byParent.set(parentId, [row]);
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

// The v4 preset hands JSON columns over as strings, and takes them back the same
// way, so the tree is only a tree between these two.
export function nodeToRow(node: {
  id: string;
  parentId?: string | null;
  ordering: unknown;
  content: unknown;
}): OutlineRow {
  return {
    id: node.id,
    parentId: node.parentId ?? null,
    ordering: Number(node.ordering),
    content: parseContent(node.content),
  };
}

function parseContent(content: unknown): Record<string, unknown> {
  return (typeof content === 'string' ? JSON.parse(content) : content) as Record<
    string,
    unknown
  >;
}

// Enough of a node to label it in a breadcrumb or a list.
export function nodeText(raw: unknown): string {
  const inline = parseContent(raw).content;
  if (!Array.isArray(inline)) {
    return '';
  }
  return inline
    .map((part: { type?: string; text?: string; props?: { label?: string; refId?: string } }) =>
      part.type === 'text' ? (part.text ?? '') : (part.props?.label || part.props?.refId || ''),
    )
    .join('')
    .trim();
}
