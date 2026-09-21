import {
  BlockNoteSchema,
  createBulletListItemBlockSpec,
  createCheckListItemBlockSpec,
  createNumberedListItemBlockSpec,
  defaultBlockSpecs,
  defaultInlineContentSpecs,
} from '@blocknote/core';
import { foldable } from './foldable-list.ts';
import { foldStore } from './folding.ts';
import { Tag } from './tags.tsx';
import { YouTubeBlock } from './YouTubeBlock.tsx';

// One store for every list item, so what is folded survives a reload and does
// not depend on which part of the tree happens to be loaded.
const folds = foldStore();

export const outlineSchema = BlockNoteSchema.create({
  blockSpecs: {
    ...defaultBlockSpecs,
    bulletListItem: foldable(createBulletListItemBlockSpec(), folds),
    numberedListItem: foldable(createNumberedListItemBlockSpec(), folds),
    checkListItem: foldable(createCheckListItemBlockSpec(), folds),
    youtube: YouTubeBlock(),
  },
  inlineContentSpecs: { ...defaultInlineContentSpecs, tag: Tag },
});
