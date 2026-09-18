'use client';

import { cn } from '@/lib/cn';
import { EditorContent, useEditor } from '@tiptap/react';
import React from 'react';
import { outlineExtensions } from './extensions.ts';
import { outlineToRows, rowsToOutline, type OutlineRow } from './rows.ts';
import { TagSuggestion, type SuggestionState, type TagCandidate } from './suggestion.ts';
import { TAG_PREFIX } from './tag-node.ts';

export type OutlineEditorProps = {
  rows: OutlineRow[];
  onChange: (rows: OutlineRow[]) => void;
  candidates: (char: string, query: string) => TagCandidate[];
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
  const [suggestion, setSuggestion] = React.useState<SuggestionState | null>(null);

  // Only the first render seeds the editor; afterwards the editor owns the tree
  // and rows flow outwards, so a save round trip does not reset the caret.
  const [initialContent] = React.useState(() => rowsToOutline(rows));

  const editor = useEditor({
    editable,
    immediatelyRender: false,
    content: initialContent,
    extensions: [
      ...outlineExtensions,
      TagSuggestion.configure({
        char: '@',
        items: (query) => candidates('@', query),
        onChange: setSuggestion,
      }),
      TagSuggestion.extend({ name: 'tagSuggestionHash' }).configure({
        char: '#',
        items: (query) => candidates('#', query),
        onChange: setSuggestion,
      }),
    ],
    onUpdate: ({ editor: instance }) => onChange(outlineToRows(instance.getJSON())),
    editorProps: {
      attributes: {
        class: cn(
          'prose-sm max-w-none outline-none',
          '[&_ul]:list-disc [&_ul]:pl-5 [&_ol]:list-decimal [&_ol]:pl-5',
          '**:data-tag:rounded **:data-tag:bg-accent-4 **:data-tag:px-1',
          '**:data-tag:text-accent-11',
        ),
      },
    },
  });

  return (
    <div className={cn('relative', className)}>
      <EditorContent editor={editor} />
      {suggestion && suggestion.items.length > 0 && (
        <TagSuggestionList state={suggestion} />
      )}
    </div>
  );
}

function TagSuggestionList({ state }: { state: SuggestionState }) {
  const { rect } = state;

  return (
    <ul
      className="fixed z-50 max-h-64 w-64 overflow-y-auto rounded-md border border-neutral-6 bg-neutral-2 py-1 shadow-lg"
      style={{ top: (rect?.bottom ?? 0) + 4, left: rect?.left ?? 0 }}
    >
      {state.items.map((item, index) => (
        <li key={`${item.kind}:${item.refId}`}>
          <button
            type="button"
            className={cn(
              'flex w-full items-baseline gap-2 px-3 py-1 text-left text-sm',
              index === state.selected ? 'bg-accent-5 text-accent-12' : 'hover:bg-neutral-4',
            )}
            onMouseDown={(event) => {
              event.preventDefault();
              state.onPick(item);
            }}
          >
            <span className="text-neutral-11">{TAG_PREFIX[item.kind]}</span>
            <span className="grow">{item.label}</span>
            <span className="text-xs text-neutral-10">{item.kind}</span>
          </button>
        </li>
      ))}
    </ul>
  );
}
