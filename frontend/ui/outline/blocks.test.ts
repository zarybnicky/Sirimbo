import assert from 'node:assert/strict';
import { describe, test } from 'node:test';
import { toBlocks } from './blocks.ts';

const paragraph = {
  type: 'paragraph',
  content: [{ type: 'text', text: 'Vítejte', styles: {} }],
};

describe('toBlocks', () => {
  test('parses the serialized document the JSON scalar hands over', () => {
    assert.deepEqual(toBlocks(JSON.stringify([paragraph])), [paragraph]);
  });

  test('takes an already parsed document as it is', () => {
    assert.deepEqual(toBlocks([paragraph]), [paragraph]);
  });

  test('gives an unwritten description something to mount on', () => {
    for (const empty of ['[]', [], null, undefined, '']) {
      assert.deepEqual(toBlocks(empty), [{ type: 'paragraph' }]);
    }
  });

  test('falls back rather than throwing on a value that is not a document', () => {
    for (const bad of ['not json', '{"type":"paragraph"}', 42]) {
      assert.deepEqual(toBlocks(bad), [{ type: 'paragraph' }]);
    }
  });
});
