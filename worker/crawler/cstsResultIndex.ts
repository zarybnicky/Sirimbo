import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import { upsertFrontierKeys } from './crawler.queries.ts';
import { endOfMonth } from 'date-fns';

const rangeKeyRe = /^(\d{4})-(\d{2})$/;

function parseRangeKey(key: string) {
  const match = rangeKeyRe.exec(key);
  if (!match) {
    throw new Error(`Invalid csts eventIndex key: ${key}`);
  }
  const month = new Date(Date.UTC(parseInt(match[1]), parseInt(match[2]), 1));
  return {
    from: month.toISOString().slice(0, 10),
    to: endOfMonth(month).toISOString().slice(0, 10),
  };
}

const responseSchema = z.object({
  collection: z.array(
    z.object({
      eventId: z.number(),
      eventTitle: z.string(),
      location: z.string(),
      dateFrom: z.iso.date(),
      dateTo: z.iso.date(),
    }),
  ),
});

type Response = z.output<typeof responseSchema>;

export const cstsResultIndex: JsonLoader<Response> = {
  mode: 'json',
  schema: responseSchema,
  revalidatePeriod: '1 day',
  buildRequest: (key) => {
    const { from, to } = parseRangeKey(key);
    return {
      url: new URL(`https://www.csts.cz/api/1/competition_events?from=${from}&to=${to}`),
      init: {
        referrer: 'https://www.csts.cz/dancesport/kalendar_akci',
      },
    };
  },
  async load(client, _frontier, parsed) {
    await upsertFrontierKeys.run(
      {
        federation: 'csts',
        kind: 'competitionResults',
        keys: parsed.collection.map((x) => x.eventId.toString()),
      },
      client,
    );
  },
};
