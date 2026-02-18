import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import { upsertCategory } from './federated.queries.ts';
import { upsertFrontier } from './crawler.queries.ts';

const Competition = z.object({
  link: z.array(
    z.object({ href: z.string(), rel: z.string(), type: z.string().optional() }),
  ),
  id: z.number(),
  name: z.string().optional(),
  discipline: z.string().optional(),
  age: z.string().optional(),
  coefficient: z.number().optional(),
  date: z.string().optional(),
  division: z.string().optional(),
  country: z.string().optional(),
  location: z.string().optional(),
  type: z.string().optional(),
  status: z.string().optional(),
});

export const wdsfCompetition: JsonLoader<z.infer<typeof Competition>> = {
  mode: 'json',
  schema: Competition,
  buildRequest: (key) => ({
    url: new URL(`https://services.worlddancesport.org/api/1/competition/${key}`),
    init: {
      headers: {
        Authorization: process.env.WDSF_AUTH ?? undefined,
        Accept: 'application/json',
      },
    },
  }),
  revalidatePeriod: '30d',
  load: async (client, _, c) => {
    const date = dateIso(c.date);
    if (!date) return;

    const categoryId = await upsertCategory.run(
      {
        series: 'wdsf',
        discipline: normDiscipline(c.discipline),
        ageGroup: normAge(c.age),
        genderGroup: 'mixed',
        class: normClass(c.type),
        competitorType: 'couple',
      },
      client,
    );
    const eventId = await upsertEvent.run(
      {
        federation: 'wdsf',
        date,
        location: txt(c.location ?? ''),
        country: normCountry(c.country ?? null),
        name: txt(c.name ?? c.location ?? '') || null,
      },
      client,
    );
    await upsertCompetition.run(
      {
        federation: 'wdsf',
        externalId: String(c.id),
        eventId,
        categoryId,
        date,
      },
      client,
    );
    await upsertFrontier.run(
      { federation: 'wdsf', kind: 'participantIndex', key: c.id },
      client,
    );
    await upsertFrontier.run(
      { federation: 'wdsf', kind: 'officialIndex', key: c.id },
      client,
    );
  },
};
