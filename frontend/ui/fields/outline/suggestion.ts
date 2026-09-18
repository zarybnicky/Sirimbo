/* eslint-disable unicorn/no-this-outside-of-class -- Tiptap's extension API is `this`-based */
import { Extension } from '@tiptap/core';
import Suggestion, { type SuggestionProps } from '@tiptap/suggestion';
import type { TagKind } from './tag-node.ts';

export type TagCandidate = {
  kind: TagKind;
  refId: string;
  label: string;
};

export type SuggestionState = {
  items: TagCandidate[];
  selected: number;
  rect: DOMRect | null;
  onPick: (candidate: TagCandidate) => void;
};

type TagSuggestionOptions = {
  char: string;
  items: (query: string) => TagCandidate[];
  onChange: (state: SuggestionState | null) => void;
};

// One instance per trigger character. Keyboard handling stays here because the
// plugin sees the keys before React does; the list itself is rendered by
// whoever passes `onChange`.
export const TagSuggestion = Extension.create<TagSuggestionOptions>({
  name: 'tagSuggestion',

  addOptions() {
    return {
      char: '@',
      items: () => [],
      onChange: () => {},
    };
  },

  addProseMirrorPlugins() {
    const { char, items, onChange } = this.options;
    let selected = 0;
    // onKeyDown only receives the view, event and range, so the latest items and
    // command are kept here between renders.
    let current: SuggestionProps<TagCandidate> | null = null;

    const publish = () => {
      if (!current) {
        return;
      }
      const props = current;
      onChange({
        items: props.items,
        selected,
        rect: props.clientRect?.() ?? null,
        onPick: (candidate) => props.command(candidate),
      });
    };

    return [
      Suggestion<TagCandidate>({
        editor: this.editor,
        char,
        items: ({ query }) => items(query),

        command: ({ editor, range, props }) => {
          editor
            .chain()
            .focus()
            .insertContentAt(range, [
              { type: 'tag', attrs: { kind: props.kind, refId: props.refId, label: props.label } },
              { type: 'text', text: ' ' },
            ])
            .run();
        },

        render: () => {
          return {
            onStart: (props) => {
              current = props;
              selected = 0;
              publish();
            },
            onUpdate: (props) => {
              current = props;
              selected = Math.min(selected, Math.max(props.items.length - 1, 0));
              publish();
            },
            onKeyDown: ({ event }) => {
              const count = current?.items.length ?? 0;
              if (event.key === 'Escape') {
                onChange(null);
                return true;
              }
              if (!current || count === 0) {
                return false;
              }
              switch (event.key) {
                case 'ArrowDown': {
                  selected = (selected + 1) % count;
                  break;
                }
                case 'ArrowUp': {
                  selected = (selected - 1 + count) % count;
                  break;
                }
                case 'Enter':
                case 'Tab': {
                  current.command(current.items[selected]!);
                  return true;
                }
                default: {
                  return false;
                }
              }
              publish();
              return true;
            },
            onExit: () => {
              current = null;
              onChange(null);
            },
          };
        },
      }),
    ];
  },
});

// Candidates that need no lookup: a fixed vocabulary, and the months around today.
export const DISCIPLINE_CANDIDATES: TagCandidate[] = [
  { kind: 'discipline', refId: 'stt', label: 'STT' },
  { kind: 'discipline', refId: 'lat', label: 'LAT' },
  { kind: 'discipline', refId: 'conditioning', label: 'Kondice' },
  { kind: 'discipline', refId: 'practice', label: 'Praxe' },
];

export function monthCandidates(today: Date, span = 12): TagCandidate[] {
  return Array.from({ length: span }, (_, offset) => {
    const date = new Date(Date.UTC(today.getFullYear(), today.getMonth() - offset, 1));
    const refId = date.toISOString().slice(0, 10);
    return { kind: 'month' as const, refId, label: refId.slice(0, 7) };
  });
}
