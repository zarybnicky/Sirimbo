import { z } from 'zod';
import { defaultMapResponseToStatus, type JsonLoader } from './types.ts';
import { upsertFrontiers } from './crawler.queries.ts';

export const CompetitionList = z.array(
  z.object({
    id: z.number(),
    name: z.string().optional(),
    lastmodifiedDate: z.string().optional(),
  }),
);

export const wdsfCompetitionIndex: JsonLoader<z.infer<typeof CompetitionList>> = {
  mode: 'json',
  schema: CompetitionList,
  buildRequest: (key) => ({
    url: new URL(`https://services.worlddancesport.org/api/1/competition?${key}`),
    init: {
      headers: {
        Authorization: process.env.WDSF_AUTH ?? undefined,
        Accept: 'application/json',
      },
    },
  }),
  revalidatePeriod: '12h',
  mapResponseToStatus(args) {
    if (!args.parsed?.length) return 'gone';
    return defaultMapResponseToStatus(args);
  },
  load: async (client, _, parsed) => {
    await upsertFrontiers.run(
      {
        federation: 'wdsf',
        kind: 'competition',
        keys: parsed.map((c) => String(c.id)),
      },
      client,
    );
  },
};
