import assert from 'node:assert/strict';
import { describe, test } from 'node:test';
import { fold, FOLD_LIMIT, unfold } from './folding.ts';

describe('folding', () => {
  test('folding a node remembers it, most recent first', () => {
    assert.deepEqual(fold(fold([], 'a'), 'b'), ['b', 'a']);
  });

  test('folding the same node twice does not duplicate it', () => {
    assert.deepEqual(fold(['b', 'a'], 'a'), ['a', 'b']);
  });

  test('unfolding forgets it', () => {
    assert.deepEqual(unfold(['b', 'a'], 'a'), ['b']);
    assert.deepEqual(unfold(['b'], 'missing'), ['b']);
  });

  test('the list is bounded, losing what was folded longest ago', () => {
    let folded: string[] = [];
    for (let n = 0; n < FOLD_LIMIT + 10; n++) {
      folded = fold(folded, `node-${n}`);
    }

    assert.equal(folded.length, FOLD_LIMIT);
    assert.equal(folded[0], `node-${FOLD_LIMIT + 9}`);
    assert.ok(!folded.includes('node-0'));
  });

  test('ids the current view cannot see are kept, since zooming hides them', () => {
    // Folded while zoomed out, then a zoomed load that never mentions them.
    const folded = fold(fold([], 'outside'), 'inside');
    assert.deepEqual(unfold(folded, 'inside'), ['outside']);
  });
});
