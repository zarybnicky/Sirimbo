'use client';

import { cn } from '@/lib/cn';
import { filterSuggestionItems } from '@blocknote/core';
import { BlockNoteView } from '@blocknote/ariakit';
import {
  getDefaultReactSlashMenuItems,
  SuggestionMenuController,
  useCreateBlockNote,
} from '@blocknote/react';
import React from 'react';
import { blocksToRows, rowsToBlocks, type OutlineRow } from './blocks.ts';
import type { TagCandidate } from './candidates.ts';
import { pauseHiddenMedia } from './pause-hidden-media.ts';
import { outlineSchema } from './schema.tsx';
import { TAG_PREFIX } from './tags.tsx';
import '@blocknote/core/style.css';
import '@blocknote/ariakit/style.css';
import './theme.css';

export type OutlineEditorProps = {
  rows: OutlineRow[];
  // Both absent in view mode: nothing is written, and no candidates are fetched
  // for a picker that can never open.
  onChange?: (rows: OutlineRow[]) => void;
  candidates?: (char: string, query: string) => Promise<TagCandidate[]>;
  uploadFile?: (file: File) => Promise<string>;
  editable?: boolean;
  className?: string;
};

export function OutlineEditor({
  rows,
  onChange,
  candidates,
  uploadFile,
  editable = true,
  className,
}: OutlineEditorProps) {
  // Only the first render seeds the editor; afterwards it owns the tree and rows
  // flow outwards, so a save round trip does not reset the caret.
  const [initialContent] = React.useState(() => rowsToBlocks(rows));

  const editor = useCreateBlockNote({
    schema: outlineSchema,
    initialContent,
    uploadFile,
  });

  // Collapsing a toggle only hides its children, so anything playing inside them
  // has to be stopped by hand until folding drops the subtree outright.
  React.useEffect(() => {
    let detach: (() => void) | undefined;
    const attach = (root: HTMLElement | undefined) => {
      detach ??= root ? pauseHiddenMedia(root) : undefined;
    };

    // Which of these lands first depends on whether the view mounted the editor
    // before this effect ran, so both are covered and the first one wins.
    attach(editor.domElement);
    const unsubscribe = editor.onMount(({ editor: mounted }) => attach(mounted.domElement));

    return () => {
      unsubscribe();
      detach?.();
    };
  }, [editor]);

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

  // The default menu knows nothing about a block added here, so this one
  // replaces it — hence `slashMenu={false}` on the view below, or the two would
  // both claim the trigger and neither would open.
  const slashItems = React.useCallback(
    async (query: string) => {
      // The block asks for the link itself; a URL typed into the query here
      // would not survive, since its own slashes and colon re-trigger the menu.
      const youtube = {
        title: 'YouTube',
        subtext: 'Vložit video',
        group: 'Media',
        onItemClick: () =>
          editor.insertBlocks(
            [{ type: 'youtube' }],
            editor.getTextCursorPosition().block,
            'after',
          ),
      };

      // Groups have to stay contiguous: the menu keys its sections by group
      // name, so appending would open a second "Media" section and collide.
      const items = getDefaultReactSlashMenuItems(editor);
      const lastOfGroup = items.findLastIndex((item) => item.group === youtube.group);
      items.splice(lastOfGroup === -1 ? items.length : lastOfGroup + 1, 0, youtube);

      return filterSuggestionItems(items, query);
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
      slashMenu={false}
      onChange={() => onChange?.(blocksToRows(editor.document))}
    >
      {editable && (
        <SuggestionMenuController
          triggerCharacter="/"
          getItems={slashItems}
          // What the default controller does, kept: `/` in a table cell is text.
          shouldOpen={({ selection }) =>
            !selection.$from.parent.type.isInGroup('tableContent')
          }
        />
      )}
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
