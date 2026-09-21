'use client';

import { cn } from '@/lib/cn';
import type { Block } from '@blocknote/core';
import { BlockNoteView } from '@blocknote/ariakit';
import { useCreateBlockNote } from '@blocknote/react';
import React from 'react';
import { toBlocks } from './blocks.ts';
import { outlineSchema } from './schema.tsx';
import '@blocknote/core/style.css';
import '@blocknote/ariakit/style.css';
import './theme.css';

export type BlockNoteDocumentProps = {
  value: unknown;
  onChange?: (value: Block[]) => void;
  onBlur?: () => void;
  editable?: boolean;
  className?: string;
};

export function BlockNoteDocument({
  value,
  onChange,
  onBlur,
  editable = true,
  className,
}: BlockNoteDocumentProps) {
  // Seeded once; the editor owns the document from then on, so a save round trip
  // does not reset the caret. A read-only view has no caret to protect.
  const [initialContent] = React.useState(() => toBlocks(value));
  const editor = useCreateBlockNote({ schema: outlineSchema, initialContent });

  React.useEffect(() => {
    if (!editable) {
      editor.replaceBlocks(editor.document, toBlocks(value));
    }
  }, [editable, editor, value]);

  return (
    <BlockNoteView
      editor={editor}
      editable={editable}
      className={cn('bn-outline', className)}
      onChange={() => onChange?.(editor.document)}
      onBlur={onBlur}
    />
  );
}
