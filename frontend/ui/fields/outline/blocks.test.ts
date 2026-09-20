import assert from 'node:assert/strict';
import { describe, test } from 'node:test';
import {
  blocksToRows,
  nodeText,
  nodeToRow,
  rowsToBlocks,
  type OutlineRow,
} from './blocks.ts';

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

  test('a subtree renders from its own root, whose parent is outside the excerpt', () => {
    const rows: OutlineRow[] = [
      { id: 'c', parentId: 'b', ordering: 1, content: block('c', text('root')) },
      { id: 'e', parentId: 'c', ordering: 1, content: block('e', text('under it')) },
    ];

    assert.deepEqual(
      blocksToRows(rowsToBlocks(rows) as unknown as Fixture[]).map((row) => [
        row.id,
        row.parentId,
      ]),
      [
        ['c', null],
        ['e', 'c'],
      ],
    );
  });
});

describe('rows off the wire', () => {
  test('content arrives as a JSON string, since that is how the API sends it', () => {
    const node = block('c', text('Warm-up'));

    assert.deepEqual(nodeToRow({ id: 'c', parentId: null, ordering: '3', content: JSON.stringify(node) }), {
      id: 'c',
      parentId: null,
      ordering: 3,
      content: node,
    });
  });

  test('a null parent comes back as null, not undefined', () => {
    assert.equal(nodeToRow({ id: 'c', ordering: 1, content: '{}' }).parentId, null);
  });
});

describe('node text', () => {
  test('reads text and tag labels, for breadcrumbs', () => {
    const node = block('c', [...text('Work on '), tag('dance', 'W', 'Waltz')]);
    assert.equal(nodeText(node), 'Work on Waltz');
    assert.equal(nodeText(JSON.stringify(node)), 'Work on Waltz');
  });

  test('falls back to the reference when a tag has no label', () => {
    assert.equal(nodeText(block('m', [tag('month', '2026-10-01', '')])), '2026-10-01');
  });

  test('is empty for a block with no inline content', () => {
    assert.equal(nodeText({ type: 'image', props: {} }), '');
  });
});
