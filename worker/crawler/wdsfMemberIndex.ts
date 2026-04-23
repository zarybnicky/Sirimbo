import type { JsonLoader } from './types.ts';
import { upsertPerson } from './federated.queries.ts';
import { z } from 'zod';
import { upsertFrontier } from './crawler.queries.ts';

export const wdsfMemberIndex: JsonLoader = {
  mode: 'json',
  revalidatePeriod: '2 day',
  buildRequest: () => ({
    url: new URL('https://services.worlddancesport.org/api/1/person'),
    init: {
      headers: {
        Authorization: process.env.WDSF_AUTH ?? undefined,
        Accept: 'application/json',
      },
    },
  }),
  schema: z.array(
    z.object({
      link: z.array(
        z.object({ href: z.string(), rel: z.string(), type: z.string().optional() }),
      ),
      id: z.string(),
      name: z.string(),
      sex: z.enum(['Male', 'Female', 'NotSupplied']),
      country: z.string().nullable(),
      ageGroup: z.string().nullable(),
    }),
  ),
  async load(client, frontier, parsed) {
    for (const member of parsed) {
      await upsertPerson.run(
        {
          federation: 'wdsf',
          externalId: member.id,
          canonicalName: member.name,
          gender:
            member.sex === 'Male'
              ? 'male'
              : member.sex === 'Female'
                ? 'female'
                : 'unknown',
        },
        client,
      );

      await upsertFrontier.run(
        { federation: 'wdsf', kind: 'member', key: member.id.toString() },
        client,
      );
    }
  },
};
