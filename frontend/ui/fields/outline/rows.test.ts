import { getSchema } from '@tiptap/core';
import { EditorState } from '@tiptap/pm/state';
import assert from 'node:assert/strict';
import { describe, test } from 'node:test';
import { outlineExtensions } from './extensions.ts';
import { nodeIdPlugin } from './node-id.ts';
import { outlineToRows, rowsToOutline, type OutlineRow } from './rows.ts';
import { tagText } from './tag-node.ts';
import { monthCandidates } from './suggestion.ts';

const schema = getSchema(outlineExtensions);

const text = (value: string) => ({
  type: 'paragraph',
  content: [{ type: 'text', text: value }],
});

const tag = (kind: string, refId: string, label: string) => ({
  type: 'tag',
  attrs: { kind, refId, label },
});

function stateWith(doc: unknown, generateId: () => string) {
  return EditorState.create({
    doc: schema.nodeFromJSON(doc),
    plugins: [nodeIdPlugin({ types: ['listItem'], attributeName: 'uuid', generateId })],
  });
}

function listItemIds(state: EditorState) {
  const ids: (string | null)[] = [];
  state.doc.descendants((node) => {
    if (node.type.name === 'listItem') {
      ids.push(node.attrs.uuid);
    }
  });
  return ids;
}

describe('outline rows', () => {
  // Depth-first, so a parent always precedes its children and the
  // self-referencing foreign key is satisfied by insertion order alone.
  const rows: OutlineRow[] = [
    { id: 'a', parentId: null, ordering: 1, listType: 'bullet', content: text('Warm-up') },
    { id: 'b', parentId: null, ordering: 2, listType: 'bullet', content: text('STT block') },
    { id: 'c', parentId: 'b', ordering: 1, listType: 'ordered', content: text('Waltz') },
    { id: 'e', parentId: 'c', ordering: 1, listType: 'bullet', content: text('Natural turn') },
    { id: 'd', parentId: 'b', ordering: 2, listType: 'ordered', content: text('Tango') },
  ];

  test('round-trips nesting, ordering and list type', () => {
    assert.deepEqual(outlineToRows(rowsToOutline(rows)), rows);
  });

  test('emits every parent before its children', () => {
    const seen = new Set<string>();
    for (const row of outlineToRows(rowsToOutline(rows))) {
      if (row.parentId !== null) {
        assert.ok(seen.has(row.parentId), `${row.id} came before its parent`);
      }
      seen.add(row.id);
    }
  });

  test('renumbers ordering from the document order', () => {
    const rows: OutlineRow[] = [
      { id: 'a', parentId: null, ordering: 40, listType: 'bullet', content: text('second') },
      { id: 'b', parentId: null, ordering: 10, listType: 'bullet', content: text('first') },
    ];

    assert.deepEqual(
      outlineToRows(rowsToOutline(rows)).map((row) => [row.id, row.ordering]),
      [
        ['b', 1],
        ['a', 2],
      ],
    );
  });

  test('rejects a list item with no uuid', () => {
    assert.throws(
      () =>
        outlineToRows({
          type: 'doc',
          content: [{ type: 'bulletList', content: [{ type: 'listItem', content: [text('x')] }] }],
        }),
      /without a uuid/,
    );
  });

  test('an empty outline produces one item the plugin can fill in', () => {
    let next = 0;
    const state = stateWith(rowsToOutline([]), () => `generated-${++next}`);
    const after = state.apply(state.tr.insertText('x', 3));

    assert.deepEqual(listItemIds(after), ['generated-1']);
  });
});

describe('tags', () => {
  const withTags = {
    type: 'doc',
    content: [
      {
        type: 'bulletList',
        content: [
          {
            type: 'listItem',
            attrs: { uuid: 'a' },
            content: [
              {
                type: 'paragraph',
                content: [
                  { type: 'text', text: 'Work on ' },
                  tag('dance', 'W', 'Waltz'),
                  { type: 'text', text: ' with ' },
                  tag('person', '42', 'Petr'),
                ],
              },
            ],
          },
        ],
      },
    ],
  };

  test('the schema keeps tag nodes instead of dropping them', () => {
    // toJSON hands back null-prototype attrs; cloning normalises them.
    const parsed = structuredClone(schema.nodeFromJSON(withTags).toJSON());
    const inline = parsed.content[0].content[0].content[0].content;

    assert.deepEqual(
      inline.filter((child: { type: string }) => child.type === 'tag'),
      [tag('dance', 'W', 'Waltz'), tag('person', '42', 'Petr')],
    );
  });

  test('tags survive the row round trip inside node content', () => {
    const rows = outlineToRows(withTags);

    assert.equal(rows.length, 1);
    assert.deepEqual(outlineToRows(rowsToOutline(rows)), rows);
    assert.deepEqual(
      rows[0]!.content.content?.filter((child) => child.type === 'tag'),
      [tag('dance', 'W', 'Waltz'), tag('person', '42', 'Petr')],
    );
  });

  test('renders a prefix per kind', () => {
    assert.equal(tagText({ kind: 'person', refId: '42', label: 'Petr' }), '@Petr');
    assert.equal(tagText({ kind: 'dance', refId: 'W', label: 'Waltz' }), '#Waltz');
    assert.equal(tagText({ kind: 'month', refId: '2026-10-01', label: null }), '#2026-10-01');
  });
});

describe('month candidates', () => {
  test('counts backwards from the current month, pinned to the first', () => {
    const months = monthCandidates(new Date('2026-02-17T12:00:00Z'), 4);

    assert.deepEqual(
      months.map((month) => month.refId),
      ['2026-02-01', '2026-01-01', '2025-12-01', '2025-11-01'],
    );
    assert.deepEqual(months[0]!.label, '2026-02');
  });
});

describe('nodeId plugin', () => {
  test('backfills items that arrive without an id', () => {
    let next = 0;
    const state = stateWith(
      {
        type: 'doc',
        content: [
          {
            type: 'bulletList',
            content: [
              { type: 'listItem', content: [text('one')] },
              { type: 'listItem', attrs: { uuid: 'kept' }, content: [text('two')] },
            ],
          },
        ],
      },
      () => `generated-${++next}`,
    );

    const after = state.apply(state.tr.insertText('!', 3));

    assert.deepEqual(listItemIds(after), ['generated-1', 'kept']);
  });

  test('a duplicated item keeps the original id and the copy gets a fresh one', () => {
    let next = 0;
    const state = stateWith(
      {
        type: 'doc',
        content: [
          {
            type: 'bulletList',
            content: [{ type: 'listItem', attrs: { uuid: 'original' }, content: [text('one')] }],
          },
        ],
      },
      () => `generated-${++next}`,
    );

    // What a paste of the same item looks like: an identical node, same attrs.
    const copy = state.doc.firstChild!.firstChild!;
    const after = state.apply(state.tr.insert(state.doc.firstChild!.nodeSize - 1, copy));

    assert.deepEqual(listItemIds(after), ['original', 'generated-1']);
  });

  test('leaves a document alone when nothing changed', () => {
    const state = stateWith(
      {
        type: 'doc',
        content: [
          {
            type: 'bulletList',
            content: [{ type: 'listItem', attrs: { uuid: 'stable' }, content: [text('one')] }],
          },
        ],
      },
      () => {
        throw new Error('should not generate an id');
      },
    );

    const after = state.apply(state.tr.insertText('!', 3));
    assert.deepEqual(listItemIds(after), ['stable']);
  });
});
