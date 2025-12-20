import type { Task } from 'graphile-worker';
import {
  getDistinctFrontierKinds,
  type IGetDistinctFrontierKindsResult,
  upsertFrontier,
} from '../crawler/crawler.queries.ts';
import { type LoaderIds } from '../crawler/handlers.ts';

const toSeed: LoaderIds[] = [
  { federation: 'wdsf', kind: 'memberIndex' },

  { federation: 'csts', kind: 'ranklistIndex' },
  { federation: 'csts', kind: 'clubIndex' },
  { federation: 'csts', kind: 'divisionIndex' },
  { federation: 'csts', kind: 'trainerIndex' },
  { federation: 'csts', kind: 'judgeIndex' },
  { federation: 'csts', kind: 'officialIndex' },

  { federation: 'szts', kind: 'competitionIndex' },
  { federation: 'szts', kind: 'memberIndex' },
  { federation: 'szts', kind: 'trainerIndex' },
  { federation: 'szts', kind: 'officialIndex' },
  { federation: 'szts', kind: 'judgeIndex' },
  { federation: 'szts', kind: 'clubIndex' },
  { federation: 'szts', kind: 'soloIndex' },
  { federation: 'szts', kind: 'coupleIndex' },
];

export const frontier_seed: Task<'frontier_seed'> = async ({}, helpers) => {
  const { withPgClient, logger } = helpers;

  await withPgClient(async (client) => {
    const entries = await getDistinctFrontierKinds.run(undefined, client);

    const key = (x: IGetDistinctFrontierKindsResult) => `${x.federation}:${x.kind}`;
    const entrySet = new Set(entries.map(key));

    for (const { federation, kind } of toSeed) {
      if (!entrySet.has(key({ federation, kind }))) {
        logger.info(`Seeding ${key({ federation, kind })}`);
        await upsertFrontier.run({ federation, kind, key: '' }, client);
      }
    }
  });
};

export default frontier_seed;
