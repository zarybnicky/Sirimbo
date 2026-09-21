import type { PartialBlock } from '@blocknote/core';

// The JSON scalar carries jsonb as a string. BlockNote will not mount on an
// empty document.
export function toBlocks(value: unknown): PartialBlock[] {
  let parsed = value;
  if (typeof parsed === 'string') {
    try {
      parsed = JSON.parse(parsed);
    } catch {
      parsed = null;
    }
  }
  return Array.isArray(parsed) && parsed.length > 0
    ? (parsed as PartialBlock[])
    : [{ type: 'paragraph' }];
}
