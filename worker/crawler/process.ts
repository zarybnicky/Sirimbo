import { zx } from '@traversable/zod';
import type { PoolClient } from 'pg';
import type { LoaderResult } from './effects.ts';
import { loaderFor } from './handlers.ts';
import { setFrontierReferenceDate } from './crawler.queries.ts';

type ProcessableFrontier = {
  federation: string;
  kind: string;
  key: string;
  content: unknown;
  reference_date?: string | null;
};

export async function loadFrontier(
  client: PoolClient,
  frontier: ProcessableFrontier,
  validation: 'loose' | 'strict',
): Promise<LoaderResult> {
  const { federation, kind, key } = frontier;
  const loader = loaderFor(federation, kind);
  if (!loader) throw new Error(`Unknown loader ${federation}:${kind}`);

  const wrapSchema = validation === 'strict' ? zx.deepStrict : zx.deepLoose;
  const content =
    loader.mode === 'json'
      ? wrapSchema(loader.schema).parse(frontier.content, { reportInput: true })
      : frontier.content;
  const result = await loader.load(client, content, key);

  if (result?.referenceDate && result.referenceDate !== frontier.reference_date) {
    await setFrontierReferenceDate.run({
      federation, kind, key, referenceDate: result.referenceDate,
    }, client);
  }
  return result;
}
