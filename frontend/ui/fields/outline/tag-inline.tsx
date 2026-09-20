import { BlockNoteSchema, defaultInlineContentSpecs } from '@blocknote/core';
import { createReactInlineContentSpec } from '@blocknote/react';

export type TagKind =
  | 'person'
  | 'couple'
  | 'cohort'
  | 'event'
  | 'competition'
  | 'dance'
  | 'month'
  | 'discipline';

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

export function tagText({
  kind,
  refId,
  label,
}: {
  kind: string;
  refId: string;
  label?: string | null;
}) {
  return `${TAG_PREFIX[kind as TagKind] ?? '@'}${label || refId}`;
}

// An atom of inline content, so the chip is one cursor step and one unit of
// deletion. The database projects these out of document_node.content; nothing
// else writes document_node_tag.
export const Tag = createReactInlineContentSpec(
  {
    type: 'tag',
    propSchema: {
      kind: { default: 'person' as TagKind },
      refId: { default: '' },
      label: { default: '' },
    },
    content: 'none',
  },
  {
    render: ({ inlineContent }) => (
      <span
        data-tag=""
        data-kind={inlineContent.props.kind}
        data-ref-id={inlineContent.props.refId}
        className="rounded bg-accent-4 px-1 text-accent-11"
      >
        {tagText(inlineContent.props)}
      </span>
    ),
  },
);

export const outlineSchema = BlockNoteSchema.create({
  inlineContentSpecs: { ...defaultInlineContentSpecs, tag: Tag },
});
