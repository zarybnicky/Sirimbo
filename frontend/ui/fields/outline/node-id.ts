import { Extension } from '@tiptap/core';
import { Plugin, PluginKey } from '@tiptap/pm/state';
import { v7 as uuidv7 } from 'uuid';

export const NODE_ID_ATTRIBUTE = 'uuid';
export const NODE_ID_TYPES = ['listItem'];

type NodeIdOptions = {
  types: string[];
  attributeName: string;
  generateId: () => string;
};

export function nodeIdPlugin({ types, attributeName, generateId }: NodeIdOptions) {
  return new Plugin({
    key: new PluginKey('nodeId'),
    appendTransaction: (transactions, _oldState, newState) => {
      if (!transactions.some((transaction) => transaction.docChanged)) {
        return null;
      }

      const seen = new Set<string>();
      const tr = newState.tr;
      let changed = false;

      // Attribute-only changes preserve node sizes, so positions collected
      // during the walk stay valid as we write.
      newState.doc.descendants((node, pos) => {
        if (!types.includes(node.type.name)) {
          return;
        }
        const id = node.attrs[attributeName];
        if (typeof id === 'string' && id && !seen.has(id)) {
          seen.add(id);
          return;
        }
        tr.setNodeAttribute(pos, attributeName, generateId());
        changed = true;
      });

      return changed ? tr : null;
    },
  });
}

// Every list item carries the primary key of its `document_node` row. `keepOnSplit`
// stops a new item created with Enter from inheriting the id it was split from;
// the plugin covers everything else that can duplicate a node — paste, drag,
// list lifting — and backfills items that arrive without an id at all.
export const NodeId = Extension.create({
  name: 'nodeId',

  addGlobalAttributes() {
    return [
      {
        types: NODE_ID_TYPES,
        attributes: {
          [NODE_ID_ATTRIBUTE]: {
            default: null,
            keepOnSplit: false,
            parseHTML: (element: HTMLElement) =>
              element.getAttribute(`data-${NODE_ID_ATTRIBUTE}`),
            renderHTML: (attributes: Record<string, unknown>) =>
              attributes[NODE_ID_ATTRIBUTE]
                ? { [`data-${NODE_ID_ATTRIBUTE}`]: attributes[NODE_ID_ATTRIBUTE] }
                : {},
          },
        },
      },
    ];
  },

  addProseMirrorPlugins() {
    return [
      nodeIdPlugin({
        types: NODE_ID_TYPES,
        attributeName: NODE_ID_ATTRIBUTE,
        generateId: uuidv7,
      }),
    ];
  },
});
