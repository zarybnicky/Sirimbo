import type { JSONContent } from '@tiptap/core';

export type OutlineListType = 'bullet' | 'ordered';

export type OutlineRow = {
  id: string;
  parentId: string | null;
  ordering: number;
  // The block this row stands for, as ProseMirror JSON, without its block-level
  // children. How finely a document splits is the editor's business; the table
  // says nothing about it.
  content: JSONContent;
};

const LIST_TYPES: Record<string, OutlineListType> = {
  bulletList: 'bullet',
  orderedList: 'ordered',
};

const emptyParagraph = (): JSONContent => ({ type: 'paragraph' });

export function listTypeOf(row: OutlineRow): OutlineListType {
  return row.content.attrs?.listType === 'ordered' ? 'ordered' : 'bullet';
}

// A `listItem` holds its own paragraph followed by an optional nested list. The
// paragraph stays on the row; the nested list becomes child rows. Which kind of
// list an item belongs to travels in its own attributes.
export function outlineToRows(doc: JSONContent): OutlineRow[] {
  const rows: OutlineRow[] = [];

  const walkList = (list: JSONContent, parentId: string | null) => {
    const listType = LIST_TYPES[list.type ?? ''] ?? 'bullet';

    for (const [index, item] of (list.content ?? []).entries()) {
      const id = item.attrs?.uuid;
      if (typeof id !== 'string' || !id) {
        throw new Error('outlineToRows: list item without a uuid attribute');
      }

      rows.push({
        id,
        parentId,
        ordering: index + 1,
        content: {
          type: 'listItem',
          attrs: { listType },
          content: [item.content?.find((child) => child.type === 'paragraph') ?? emptyParagraph()],
        },
      });

      for (const child of item.content ?? []) {
        if (child.type && child.type in LIST_TYPES) {
          walkList(child, id);
        }
      }
    }
  };

  for (const child of doc.content ?? []) {
    if (child.type && child.type in LIST_TYPES) {
      walkList(child, null);
    }
  }

  return rows;
}

export function rowsToOutline(rows: OutlineRow[]): JSONContent {
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

  const buildList = (parentId: string | null): JSONContent | null => {
    const siblings = byParent.get(parentId);
    if (!siblings?.length) {
      return null;
    }
    return {
      type: listTypeOf(siblings[0]!) === 'ordered' ? 'orderedList' : 'bulletList',
      content: siblings.map((row) => {
        const paragraph = row.content.content?.[0] ?? emptyParagraph();
        const nested = buildList(row.id);
        return {
          type: 'listItem',
          attrs: { uuid: row.id },
          content: nested ? [paragraph, nested] : [paragraph],
        };
      }),
    };
  };

  const root = buildList(null);
  // An outline with no rows starts as a single empty item; `NodeId` fills in its
  // uuid on the first transaction.
  return {
    type: 'doc',
    content: [
      root ?? {
        type: 'bulletList',
        content: [{ type: 'listItem', content: [emptyParagraph()] }],
      },
    ],
  };
}
