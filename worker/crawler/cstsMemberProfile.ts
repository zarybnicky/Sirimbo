import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import { upsertCstsMemberProfile } from './federated.queries.ts';

const schema = z.object({
  entity: z.object({
    idt: z.number().int().positive(),
    name: z.string(),
    surname: z.string(),
    title: z.string().nullish(),
    clubId: z.number().int().nullish(),
    clubName: z.string().nullish(),
    marketingInfoConsent: z.boolean().optional(),
    divisionId: z.number().int().nullish(),
    divisionName: z.string().nullish(),
  }),
});

export const cstsMemberProfile: JsonLoader<z.output<typeof schema>> = {
  mode: 'json',
  schema,
  buildRequest: (key) => ({
    url: new URL(`https://www.csts.cz/api/members/${key}`),
    init: {
      headers: {
        Referer: 'https://www.csts.cz/dancesport/kalendar_akci',
      },
    },
  }),
  async load(client, { entity: member }, key) {
    await upsertCstsMemberProfile.run(
      {
        externalId: key,
        canonicalName: [member.name, member.surname].join(' ').trim(),
        firstName: member.name,
        lastName: member.surname,
        clubExternalId: member.clubId?.toString() ?? '',
        clubName: member.clubName ?? '',
      },
      client,
    );
  },
};
