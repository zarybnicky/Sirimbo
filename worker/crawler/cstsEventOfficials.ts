import { z } from 'zod';
import type { JsonLoader } from './types.ts';

const officialSchema = z.object({
  id: z.number(),
  idt: z.number(),
  eventId: z.number(),
  eventCompId: z.number(),
  dayOfEvent: z.number(),
  name: z.string(),
  surname: z.string(),
  countryCode: z.string(),
  state: z.number(),
  approved: z.boolean(),
  role: z.number(),
  date: z.iso.datetime({ local: true }),
});

const responseSchema = z.object({
  collection: z.array(officialSchema),
});

type Response = z.output<typeof responseSchema>;

export const cstsEventOfficials: JsonLoader<Response> = {
  mode: 'json',
  schema: responseSchema,
  revalidatePeriod: '1 day',
  buildRequest: (key) => {
    return {
      url: new URL(`https://www.csts.cz/api/1/events/${key}/officials`),
      init: {
        referrer: 'https://www.csts.cz/dancesport/kalendar_akci',
      },
    };
  },
  async load(client, _frontier, parsed) {
    for (const official of parsed.collection) {
      // What's what in a role? Porota, vedoucí, dozor, sčitatel = 1, 2, 3, 4, 5?
      // A kam to uložit??? event_
    }
  },
};
