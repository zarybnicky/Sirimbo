import assert from 'node:assert/strict';
import { describe, test } from 'node:test';
import { blocksToRows, rowsToBlocks, type OutlineRow } from './blocks.ts';

const text = (value: string) => [{ type: 'text', text: value, styles: {} }];

const tag = (kind: string, refId: string, label: string) => ({
  type: 'tag',
  props: { kind, refId, label },
});

type Fixture = {
  id: string;
  type: string;
  props: Record<string, unknown>;
  content: unknown;
  children: Fixture[];
};

const block = (id: string, content: unknown, children: Fixture[] = []): Fixture => ({
  id,
  type: 'bulletListItem',
  props: {},
  content,
  children,
});

describe('outline blocks', () => {
  const document = [
    block('a', text('Warm-up')),
    block('b', text('STT block'), [
      { ...block('c', [...text('Work on '), tag('dance', 'W', 'Waltz')]), type: 'numberedListItem' },
      { ...block('d', text('Tango')), type: 'numberedListItem' },
    ]),
  ];

  test('round-trips nesting, ordering and block type', () => {
    const rows = blocksToRows(document);
    assert.deepEqual(blocksToRows(rowsToBlocks(rows) as unknown as Fixture[]), rows);
  });

  test('a block keeps its own type; nesting is the row tree', () => {
    const rows = blocksToRows(document);

    assert.deepEqual(
      rows.map((row) => [row.id, row.parentId, row.ordering, row.content.type]),
      [
        ['a', null, 1, 'bulletListItem'],
        ['b', null, 2, 'bulletListItem'],
        ['c', 'b', 1, 'numberedListItem'],
        ['d', 'b', 2, 'numberedListItem'],
      ],
    );
  });

  test('emits every parent before its children', () => {
    const seen = new Set<string>();
    for (const row of blocksToRows(document)) {
      if (row.parentId !== null) {
        assert.ok(seen.has(row.parentId), `${row.id} came before its parent`);
      }
      seen.add(row.id);
    }
  });

  test('renumbers ordering from document order', () => {
    const rows: OutlineRow[] = [
      { id: 'a', parentId: null, ordering: 40, content: block('a', text('second')) },
      { id: 'b', parentId: null, ordering: 10, content: block('b', text('first')) },
    ];

    assert.deepEqual(
      blocksToRows(rowsToBlocks(rows) as unknown as Fixture[]).map((row) => [row.id, row.ordering]),
      [
        ['b', 1],
        ['a', 2],
      ],
    );
  });

  test('tags ride inside a block, which is what the trigger reads', () => {
    const rows = blocksToRows(document);
    const tagged = rows.find((row) => row.id === 'c')!;

    assert.deepEqual(
      (tagged.content.content as { type: string }[]).filter((part) => part.type === 'tag'),
      [tag('dance', 'W', 'Waltz')],
    );
  });

  test('an empty outline seeds one item, since BlockNote will not mount on nothing', () => {
    assert.deepEqual(rowsToBlocks([]), [{ type: 'bulletListItem' }]);
  });
});
