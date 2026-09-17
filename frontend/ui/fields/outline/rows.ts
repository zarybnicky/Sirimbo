import type { JSONContent } from '@tiptap/core';

export type OutlineListType = 'bullet' | 'ordered';

export type OutlineRow = {
  id: string;
  parentId: string | null;
  ordering: number;
  listType: OutlineListType;
  content: JSONContent;
};

const LIST_TYPES: Record<string, OutlineListType> = {
  bulletList: 'bullet',
  orderedList: 'ordered',
};

const emptyParagraph = (): JSONContent => ({ type: 'paragraph' });

// A `listItem` holds its own paragraph followed by an optional nested list. The
// paragraph becomes `document_node.content`; the nested list becomes child rows.
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
        listType,
        content: item.content?.find((child) => child.type === 'paragraph') ?? emptyParagraph(),
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
      type: siblings[0]!.listType === 'ordered' ? 'orderedList' : 'bulletList',
      content: siblings.map((row) => {
        const nested = buildList(row.id);
        return {
          type: 'listItem',
          attrs: { uuid: row.id },
          content: nested ? [row.content, nested] : [row.content],
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
