import assert from 'node:assert/strict';
import { describe, test } from 'node:test';
import { youtubeVideoId } from './youtube.ts';

describe('youtube video id', () => {
  const id = 'VsAgAfc9ZM4';

  test('reads the shapes YouTube hands out', () => {
    for (const input of [
      id,
      `https://www.youtube.com/watch?v=${id}`,
      `https://www.youtube.com/watch?app=desktop&v=${id}&t=42s`,
      `https://youtu.be/${id}`,
      `https://youtu.be/${id}?t=42`,
      `https://www.youtube.com/embed/${id}`,
      `https://www.youtube.com/shorts/${id}`,
      `https://www.youtube.com/live/${id}`,
      `  https://youtu.be/${id}  `,
    ]) {
      assert.equal(youtubeVideoId(input), id, input);
    }
  });

  test('refuses anything that is not a video', () => {
    for (const input of [
      '',
      'https://vimeo.com/123456',
      'https://www.youtube.com/@someChannel',
      'https://example.com/watch?v=short',
      'not a url at all',
    ]) {
      assert.equal(youtubeVideoId(input), null, input);
    }
  });
});
