import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import { upsertFrontiers } from './crawler.queries.ts';

export const ParticipantsList = z.array(
  z.object({
    id: z.number(),
    name: z.string().optional(),
    country: z.string().optional(),
    number: z.string().optional(),
  }),
);

export const wdsfParticipantIndex: JsonLoader<z.infer<typeof ParticipantsList>> = {
  mode: 'json',
  schema: ParticipantsList,
  buildRequest: (key) => ({
    url: new URL(
      `https://services.worlddancesport.org/api/1/participant?competitionID=${encodeURIComponent(key)}`,
    ),
    init: {
      headers: {
        Authorization: process.env.WDSF_AUTH ?? undefined,
        Accept: 'application/json',
      },
    },
  }),
  revalidatePeriod: '7d',
  load: async (client, _, parsed) => {
    await upsertFrontiers.run(
      {
        federation: 'wdsf',
        kind: 'participant',
        keys: parsed.map((p) => String(p.id)),
      },
      client,
    );
  },
};
