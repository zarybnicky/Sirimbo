import { z } from 'zod';
import { endOfMonth } from 'date-fns';
import type { JsonLoader } from './types.ts';

const schema = z.object({
  collection: z.array(
    z.object({
      eventId: z.number(),
      dateFrom: z.iso.date(),
      dateTo: z.iso.date(),
      eventTitle: z.string(),
      location: z.string().nullish(),
    }),
  ),
  paging: z
    .object({
      page: z.number(),
      pageSize: z.number(),
      totalCount: z.number(),
    })
    .optional(),
});

export const cstsCompetitionEventIndex: JsonLoader<z.infer<typeof schema>> = {
  mode: 'json',
  schema,
  revalidatePeriod: '1 day',
  buildRequest(key) {
    if (!/^\d{4}-\d{2}$/.test(key)) {
      throw new Error(`Invalid csts competitionEventIndex key: ${key}`);
    }
    const month = new Date(`${key}-01T00:00:00Z`);
    const url = new URL('https://www.csts.cz/api/1/competition_events');
    url.searchParams.set('from', month.toISOString().slice(0, 10));
    url.searchParams.set('to', endOfMonth(month).toISOString().slice(0, 10));
    return {
      url,
      init: { headers: { Referer: 'https://www.csts.cz/dancesport/vysledky_soutezi' } },
    };
  },
  async load(_client, parsed) {
    if (parsed.paging && parsed.paging.totalCount > parsed.collection.length) {
      throw new Error('ČSTS competition event index response is incomplete');
    }
    // Results can exist without a planning/calendar record. Both indexes feed
    // the same event loader, which already reads planning and archived results.
    return {
      upsertFrontier: parsed.collection.map(({ eventId }) => ({
        federation: 'csts',
        kind: 'event',
        key: eventId.toString(),
      })),
    };
  },
};
