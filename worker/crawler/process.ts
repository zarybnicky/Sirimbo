import { zx } from '@traversable/zod';
import type { PoolClient } from 'pg';
import type { LoaderResult } from './effects.ts';
import { loaderFor } from './handlers.ts';

type ProcessableFrontier = {
  federation: string;
  kind: string;
  content: unknown;
};

export async function loadFrontier(
  client: PoolClient,
  frontier: ProcessableFrontier,
  validation: 'loose' | 'strict',
): Promise<LoaderResult> {
  const { federation, kind } = frontier;
  const loader = loaderFor(federation, kind);
  if (!loader) throw new Error(`Unknown loader ${federation}:${kind}`);

  const wrapSchema = validation === 'strict' ? zx.deepStrict : zx.deepLoose;
  const content =
    loader.mode === 'json'
      ? wrapSchema(loader.schema).parse(frontier.content, { reportInput: true })
      : frontier.content;
  return loader.load(client, content);
}
