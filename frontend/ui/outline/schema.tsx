import {
  BlockNoteSchema,
  defaultBlockSpecs,
  defaultInlineContentSpecs,
} from '@blocknote/core';

// The schema every BlockNote document is built on; custom blocks slot in here.
export const outlineSchema = BlockNoteSchema.create({
  blockSpecs: { ...defaultBlockSpecs },
  inlineContentSpecs: { ...defaultInlineContentSpecs },
});
