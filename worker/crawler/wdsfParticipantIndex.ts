import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import { upsertFrontierKeys } from './crawler.queries.ts';

export const schema = z.array(
  z.discriminatedUnion('Kind', [
    z.object({
      Kind: z.literal("Team"),
      link: z.array(
        z.object({ href: z.string(), rel: z.string(), type: z.string() })
      ),
      Team: z.string().optional(),
      TeamCountryXmlName: z.string().optional(),
      id: z.number(),
      number: z.number(),
      status: z.string(),
      nationalreference: z.string().nullish(),
    }),
    z.object({
      Kind: z.undefined(),
      link: z.array(
        z.object({ href: z.string(), rel: z.string(), type: z.string() })
      ),
      id: z.number(),
      name: z.string(),
      country: z.string(),
      number: z.number(),
      status: z.string(),
      nationalreference: z.string().nullish(),
    })
  ]),
)

export const wdsfParticipantIndex: JsonLoader<z.infer<typeof schema>> = {
  mode: 'json',
  schema,
  buildRequest: (key) => ({
    url: new URL(`https://services.worlddancesport.org/api/1/participant?competitionID=${key}`),
    init: {
      headers: {
        Authorization: process.env.WDSF_AUTH ?? '',
        Accept: 'application/json',
      },
    },
  }),
  revalidatePeriod: '7d',
  async load(client, parsed) {
    // Competitor reference needs to be fron link
    // participant Id == marks
    // name can serve as a backup competitor name

    await upsertFrontierKeys.run(
      {
        federation: 'wdsf',
        kind: 'participant',
        keys: parsed.map((p) => String(p.id)),
      },
      client,
    );
  },
};
