import type { JsonLoader } from './types.ts';
import { type gender, upsertPeople } from './federated.queries.ts';
import { z } from 'zod';
import { upsertFrontierKeys } from './crawler.queries.ts';
import { makePgtypedCollection } from './pgtypedCollection.ts';

const requestSchema = z.array(
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
);

export const wdsfMemberIndex: JsonLoader<z.output<typeof requestSchema>> = {
  mode: 'json',
  revalidatePeriod: '2 day',
  buildRequest: () => ({
    url: new URL('https://services.worlddancesport.org/api/1/person'),
    init: {
      headers: {
        Authorization: process.env.WDSF_AUTH!,
        Accept: 'application/json',
      },
    },
  }),
  schema: requestSchema,
  async load(client, parsed) {
    const people = makePgtypedCollection<{
      federation: string;
      externalId: string;
      canonicalName: string;
      gender: gender;
    }>(
      ['federation', 'externalId', 'canonicalName', 'gender'],
      ['federation', 'externalId'],
    );
    for (const member of parsed) {
      people.add({
        federation: 'wdsf',
        externalId: member.id,
        canonicalName: member.name,
        gender:
          member.sex === 'Male' ? 'male' : member.sex === 'Female' ? 'female' : 'unknown',
      });
    }
    await upsertPeople.run(people.params, client);
    await upsertFrontierKeys.run(
      {
        federation: 'wdsf',
        kind: 'member',
        keys: parsed.map((x) => x.id),
      },
      client,
    );
  },
};
