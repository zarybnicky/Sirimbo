import { z } from 'zod';
import type { PoolClient } from 'pg';

export const WdsfAgeGroupRuleSchema = z.object({
  name: z.string(),
  fromAge: z.number().int().nullable().optional(),
  toAge: z.number().int().nullable().optional(),
  minBirthdate: z.string().datetime(),
  maxBirthdate: z.string().datetime(),
  AllowedToDanceIn: z.array(z.string()).default([]),
  divisions: z.array(z.string()).default([]),
  isPerson: z.boolean(),
  isCouple: z.boolean(),
  isTeam: z.boolean(),
  isCompetition: z.boolean(),
});

export const WdsfAgeGroupRulesSchema = z.array(WdsfAgeGroupRuleSchema);
export type WdsfAgeGroupRule = z.infer<typeof WdsfAgeGroupRuleSchema>;

/**
 * Bulk-upsert WDSF age-group rules using typed arrays + UNNEST.
 *
 * valid_from/valid_to:
 * - For a yearly snapshot, pass valid_from = 'YYYY-01-01', valid_to = 'YYYY-12-31'.
 * - Or keep the default single snapshot by passing valid_from = '1900-01-01' and valid_to = null.
 */
export async function upsertWdsfAgeGroupRules(
  client: PoolClient,
  parsed: WdsfAgeGroupRule[],
  validFrom: string,
  validTo: string | null,
): Promise<void> {
  const federation = 'wdsf';

  const name: string[] = [];
  const fromAge: (number | null)[] = [];
  const toAge: (number | null)[] = [];
  const minBirthdate: string[] = [];
  const maxBirthdate: string[] = [];
  const allowedToDanceIn: string[][] = [];
  const divisions: string[][] = [];

  for (const r of parsed) {
    name.push(r.name);
    fromAge.push(r.fromAge ?? null);
    toAge.push(r.toAge ?? null);
    minBirthdate.push(r.minBirthdate.slice(0, 10));
    maxBirthdate.push(r.maxBirthdate.slice(0, 10));
    allowedToDanceIn.push(r.AllowedToDanceIn ?? []);
    divisions.push(r.divisions ?? []);
  }

  await client.query(``, [
    federation,
    validFrom,
    validTo,
    name,
    fromAge,
    toAge,
    minBirthdate,
    maxBirthdate,
    allowedToDanceIn,
    divisions,
  ]);
}
