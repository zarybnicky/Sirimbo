import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import { upsertFrontiers } from './crawler.queries.ts';

const responseSchema = z.object({
  collection: z.array(
    z.object({
      id: z.number(),
      state: z.number(),
      competitorType: z.number(),
      series: z.number(),
      date: z.string(),
      type: z.number(),
      age: z.number(),
      discipline: z.number(),
      coefChampionship: z.number(),
      coefLeague: z.number(),
      coefWdsf: z.number(),
    }),
  ),
});

export const cstsRanklistIndex: JsonLoader<z.output<typeof responseSchema>> = {
  mode: 'json',
  revalidatePeriod: '1 day',
  buildRequest: () => ({
    url: new URL('https://www.csts.cz/api/1/ranklist'),
    init: {
      headers: {
        referrer: 'https://www.csts.cz/dancesport/kalendar_akci',
      },
    },
  }),
  schema: responseSchema,
  async load(client, frontier, parsed) {
    await upsertFrontiers.run(
      {
        federation: 'csts',
        kind: 'ranklist',
        keys: parsed.collection.map((x) => x.id.toString()),
      },
      client,
    );
  },
};
