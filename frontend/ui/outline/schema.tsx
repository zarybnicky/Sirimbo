import { BlockNoteSchema, defaultBlockSpecs, defaultInlineContentSpecs } from '@blocknote/core';
import { Tag } from './tags.tsx';
import { YouTubeBlock } from './YouTubeBlock.tsx';

export const outlineSchema = BlockNoteSchema.create({
  blockSpecs: { ...defaultBlockSpecs, youtube: YouTubeBlock() },
  inlineContentSpecs: { ...defaultInlineContentSpecs, tag: Tag },
});
