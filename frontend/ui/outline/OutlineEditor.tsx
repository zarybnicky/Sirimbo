'use client';

import { cn } from '@/lib/cn';
import { BlockNoteView } from '@blocknote/ariakit';
import { SuggestionMenuController, useCreateBlockNote } from '@blocknote/react';
import React from 'react';
import { blocksToRows, rowsToBlocks, type OutlineRow } from './blocks.ts';
import type { TagCandidate } from './candidates.ts';
import { outlineSchema, TAG_PREFIX } from './tags.tsx';
import '@blocknote/core/style.css';
import '@blocknote/ariakit/style.css';
import './theme.css';

export type OutlineEditorProps = {
  rows: OutlineRow[];
  // Both absent in view mode: nothing is written, and no candidates are fetched
  // for a picker that can never open.
  onChange?: (rows: OutlineRow[]) => void;
  candidates?: (char: string, query: string) => Promise<TagCandidate[]>;
  editable?: boolean;
  className?: string;
};

export function OutlineEditor({
  rows,
  onChange,
  candidates,
  editable = true,
  className,
}: OutlineEditorProps) {
  // Only the first render seeds the editor; afterwards it owns the tree and rows
  // flow outwards, so a save round trip does not reset the caret.
  const [initialContent] = React.useState(() => rowsToBlocks(rows));

  const editor = useCreateBlockNote({
    schema: outlineSchema,
    initialContent,
  });

  // A view only editor has no caret to protect, so it follows the rows it is
  // given; an editable one would fight whoever is typing.
  React.useEffect(() => {
    if (!editable) {
      editor.replaceBlocks(editor.document, rowsToBlocks(rows));
    }
  }, [editable, editor, rows]);

  const insertTag = React.useCallback(
    (candidate: TagCandidate) => {
      editor.insertInlineContent([
        { type: 'tag', props: { ...candidate } },
        ' ',
      ]);
    },
    [editor],
  );

  const items = React.useCallback(
    async (char: string, query: string) => {
      const found = await (candidates?.(char, query) ?? []);
      return found.map((candidate) => ({
        title: `${TAG_PREFIX[candidate.kind]}${candidate.label}`,
        subtext: candidate.kind,
        onItemClick: () => insertTag(candidate),
      }));
    },
    [candidates, insertTag],
  );

  return (
    <BlockNoteView
      editor={editor}
      editable={editable}
      className={cn('bn-outline', className)}
      onChange={() => onChange?.(blocksToRows(editor.document))}
    >
      {editable && candidates && (
        <>
          <SuggestionMenuController
            triggerCharacter="@"
            getItems={(query) => items('@', query)}
          />
          <SuggestionMenuController
            triggerCharacter="#"
            getItems={(query) => items('#', query)}
          />
        </>
      )}
    </BlockNoteView>
  );
}
