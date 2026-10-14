import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import { mergeWdsfAgeGroups, mergeWdsfAgeGroupAllowances, mergeWdsfSeniorRules } from './federated.queries.ts';

const ageGroupSchema = z.object({
  name: z.string(),
  fromAge: z.number().int().nonnegative().nullable(),
  toAge: z.number().int().nonnegative().nullable(),
  minBirthdate: z.string().nullable(),
  maxBirthdate: z.string().nullable(),
  AllowedToDanceIn: z.array(z.string()),
  divisions: z.array(z.string()),
  isPerson: z.boolean(),
  isCouple: z.boolean(),
  isTeam: z.boolean(),
  isCompetition: z.boolean(),
});

export const wdsfAgeGroups: JsonLoader<z.output<typeof ageGroupSchema>[]> = {
  mode: 'json',
  schema: z.array(ageGroupSchema).min(1),
  revalidatePeriod: '30 days',
  buildRequest: () => ({
    url: new URL('https://services.worlddancesport.org/api/1/age?format=json'),
    init: {
      headers: { Authorization: process.env.WDSF_AUTH!, Accept: 'application/json' },
    },
  }),
  async load(client, parsed) {
    await mergeWdsfAgeGroups.run({ groups: JSON.stringify(parsed) }, client);
    await mergeWdsfAgeGroupAllowances.run({ groups: JSON.stringify(parsed) }, client);
    await mergeWdsfSeniorRules.run(undefined, client);
  },
};
