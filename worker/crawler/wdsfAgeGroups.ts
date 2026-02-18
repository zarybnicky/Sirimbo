import { z } from 'zod';
import type { JsonLoader } from './types.ts';

const Federation = 'wdsf' as const;

const zAgeGroup = z.object({
  name: z.string(),
  fromAge: z.number().nullable().optional(),
  toAge: z.number().nullable().optional(),
  minBirthdate: z.string().nullable().optional(),
  maxBirthdate: z.string().nullable().optional(),
  AllowedToDanceIn: z.array(z.string()).default([]),
  divisions: z.array(z.string()).default([]),
  isPerson: z.boolean().default(false),
  isCouple: z.boolean().default(false),
  isTeam: z.boolean().default(false),
  isCompetition: z.boolean().default(false),
});

const zPayload = z.array(zAgeGroup);

type Payload = z.infer<typeof zPayload>;

function toDateOnly(s: string | null | undefined): string | null {
  if (!s) return null;
  // incoming is ISO with time; keep YYYY-MM-DD
  const m = s.match(/^\d{4}-\d{2}-\d{2}/);
  return m ? m[0] : null;
}

export const wdsfAgeGroups: JsonLoader<Payload> = {
  mode: 'json',
  schema: zPayload,
  buildRequest: (_key: string) => ({
    url: new URL(`https://services.worlddancesport.org/api/1/age?format=json`),
    init: { method: 'GET', headers: { Accept: 'application/json' } },
  }),
  revalidatePeriod: '30d',

  load: async (client, _, parsed) => {
    const asOfYear = new Date().getUTCFullYear();

    for (const g of parsed) {
      await client.query(
        `
        INSERT INTO federated.federation_age_group_rule (
          federation, as_of_year, name, from_age, to_age,
          min_birthdate, max_birthdate, allowed_to_dance_in, divisions
        )
        VALUES ($1, $2, $3, $4, $5, $6::date, $7::date, $8::text[], $9::text[])
        ON CONFLICT (federation, as_of_year, name)
          DO UPDATE SET
            from_age = EXCLUDED.from_age,
            to_age = EXCLUDED.to_age,
            min_birthdate = EXCLUDED.min_birthdate,
            max_birthdate = EXCLUDED.max_birthdate,
            allowed_to_dance_in = EXCLUDED.allowed_to_dance_in,
            divisions = EXCLUDED.divisions
        `,
        [
          Federation,
          asOfYear,
          g.name,
          g.fromAge ?? null,
          g.toAge ?? null,
          toDateOnly(g.minBirthdate),
          toDateOnly(g.maxBirthdate),
          g.AllowedToDanceIn,
          g.divisions,
        ],
      );
    }
  },
};
