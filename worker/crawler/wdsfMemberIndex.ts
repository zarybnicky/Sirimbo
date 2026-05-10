import type { JsonLoader } from './types.ts';
import { ensurePeople, type gender } from './federated.queries.ts';
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
    activePartner: z.string().optional(),
    activeCoupleId: z.string().optional(),
    activeCoupleAgeGroup: z.string().optional(),
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
        gender: wdsfIndexGender(member.sex),
      });
    }
    await ensurePeople.run(people.params, client);
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

function wdsfIndexGender(value: z.output<typeof requestSchema>[number]['sex']): gender {
  switch (value) {
    case 'Male':
      return 'male';
    case 'Female':
      return 'female';
    case 'NotSupplied':
      return 'unknown';
  }
}
