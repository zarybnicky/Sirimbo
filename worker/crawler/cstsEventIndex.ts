import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import { upsertFrontier } from './crawler.queries.ts';
import { endOfMonth } from 'date-fns';
import { upsertEvent } from './federated.queries.ts';

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

const eventSchema = z.object({
  id: z.number(),
  dateFrom: z.iso.date(),
  dateTo: z.iso.date(),
  eventCompetitions: z.array(
    z.object({
      id: z.number(),
      city: z.string(),
      name: z.string(),
      date: z.iso.datetime({ local: true }),
      state: z.number(),
    }),
  ),
});

const responseSchema = z.object({
  collection: z.array(eventSchema),
});

type Response = z.output<typeof responseSchema>;

export const cstsEventIndex: JsonLoader<Response> = {
  mode: 'json',
  schema: responseSchema,
  revalidatePeriod: '1 day',
  buildRequest: (key) => {
    const { from, to } = parseRangeKey(key);
    const url = new URL('https://www.csts.cz/api/1/events');
    url.searchParams.set('filter', `date>=${from} AND date<=${to}`);
    return {
      url,
      init: {
        referrer: 'https://www.csts.cz/dancesport/kalendar_akci',
      },
    };
  },
  async load(client, _frontier, parsed) {
    for (const event of parsed.collection) {
      const key = event.id.toString();
      const competitionEvent = event.eventCompetitions.find((x) => x)!;

      await upsertEvent.run(
        {
          federation: 'csts',
          externalId: key,
          country: 'Czechia',
          startDate: competitionEvent.date,
          location: competitionEvent.city,
          name: competitionEvent.name,
        },
        client,
      );

      await upsertFrontier.run(
        { federation: 'csts', kind: 'eventCompetitions', key },
        client,
      );
      await upsertFrontier.run(
        { federation: 'csts', kind: 'eventCompetitors', key },
        client,
      );
      await upsertFrontier.run(
        { federation: 'csts', kind: 'eventOfficials', key },
        client,
      );
    }
  },
};
