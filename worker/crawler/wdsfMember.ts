import type { JsonLoader } from './types.ts';
import { upsertFederationAthlete } from './federated.queries.ts';
import { z } from 'zod';

const Member = z.object({
  link: z.array(
    z.object({ href: z.string(), rel: z.string(), type: z.string().optional() }),
  ),
  id: z.number(),
  nickname: z.string().nullable(),
  name: z.string(),
  surname: z.string().nullable(),
  sex: z.enum(['Male', 'Female', '']),
  nationality: z.string().nullable(),
  country: z.string().nullable(),
  ageGroup: z.string().nullable(),
  yearOfBirth: z.number(),
  nationalReference: z.string().nullable(),
  licenses: z.array(
    z.object({
      type: z.enum([
        'Examiner',
        'Chairman',
        'Scrutiny',
        'Athlete',
        'Adjudicator',
        'DJ',
        'HeadJudge',
        'Invigilator',
      ]),
      status: z.enum(['Active', 'Expired', 'Revoked', 'Resting', 'Retired', 'Aspiring']),
      division: z.enum([
        'Stage',
        'Smooth',
        'Disco',
        'SoloSyncroChoreo',
        'General',
        'Caribbean',
        'Professional',
        'Breaking',
        'HipHop',
        'Unknown',
      ]),
      disciplines: z.array(z.string()),
      grade: z.enum(['A', 'B']).optional(),
      expiresOn: z.string().optional(),
    }),
  ),
});

export const wdsfMember: JsonLoader<z.infer<typeof Member>> = {
  mode: 'json',
  revalidatePeriod: '5 day',
  buildRequest: (key) => ({
    url: new URL(`https://services.worlddancesport.org/api/1/person/${key}`),
    init: {
      headers: {
        Authorization: process.env.WDSF_AUTH ?? undefined,
        Accept: 'application/json',
      },
    },
  }),
  schema: Member,
  async load(client, frontier, member) {
    // we need a person -> federation person mapping, to support the various officials without duplicates
    // federation_athlete, federation_judge will point to that and signify a licence

    await upsertFederationAthlete.run(
      {
        federation: 'wdsf',
        externalId: member.id.toString(),
        canonicalName: [member.name, member.surname].join(' ').trim(),
        gender:
          member.sex === 'Male' ? 'male' : member.sex === 'Female' ? 'female' : 'unknown',
      },
      client,
    );
  },
};
