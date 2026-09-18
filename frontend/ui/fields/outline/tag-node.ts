import { Node, mergeAttributes } from '@tiptap/core';

export const TAG_KINDS = [
  'person',
  'couple',
  'cohort',
  'event',
  'competition',
  'dance',
  'month',
  'discipline',
] as const;

export type TagKind = (typeof TAG_KINDS)[number];

// `@` picks out someone or something with a page of its own; `#` classifies.
export const TAG_PREFIX: Record<TagKind, string> = {
  person: '@',
  couple: '@',
  cohort: '@',
  event: '@',
  competition: '#',
  dance: '#',
  month: '#',
  discipline: '#',
};

export type TagAttributes = {
  kind: TagKind;
  refId: string;
  label: string | null;
};

export function tagText({ kind, refId, label }: TagAttributes) {
  return `${TAG_PREFIX[kind] ?? '@'}${label ?? refId}`;
}

// An atom, so the whole chip is one cursor step and one unit of deletion. The
// database projects these into document_node_tag; nothing else writes them.
export const Tag = Node.create({
  name: 'tag',
  group: 'inline',
  inline: true,
  atom: true,
  selectable: true,

  addAttributes() {
    return {
      kind: {
        default: null,
        parseHTML: (element: HTMLElement) => element.dataset.kind,
        renderHTML: (attributes: Record<string, unknown>) => ({ 'data-kind': attributes.kind }),
      },
      refId: {
        default: null,
        parseHTML: (element: HTMLElement) => element.dataset.refId,
        renderHTML: (attributes: Record<string, unknown>) => ({ 'data-ref-id': attributes.refId }),
      },
      label: {
        default: null,
        parseHTML: (element: HTMLElement) => element.dataset.label,
        renderHTML: (attributes: Record<string, unknown>) =>
          attributes.label ? { 'data-label': attributes.label } : {},
      },
    };
  },

  parseHTML() {
    return [{ tag: 'span[data-tag]' }];
  },

  renderHTML({ HTMLAttributes, node }) {
    return [
      'span',
      mergeAttributes({ 'data-tag': '' }, HTMLAttributes),
      tagText(node.attrs as TagAttributes),
    ];
  },

  renderText({ node }) {
    return tagText(node.attrs as TagAttributes);
  },
});
