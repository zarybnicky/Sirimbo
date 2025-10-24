import { z } from 'zod';
import { MAX_IDT, MIN_IDT } from './idts.ts';

const rankingPointsSchema = z.object({
  rankingPointsAge: z.string(),
  medicalCheckupExpiration: z.string().optional().nullable().prefault(null),
  personalClass: z.string().optional().prefault('-'),
  personalPoints: z.number().optional().prefault(0),
  personalDomesticFinaleCount: z.number().optional().prefault(0),
  personalForeignFinaleCount: z.number().optional().prefault(0),
  competitorId: z.number(),
  series: z.string(),
  discipline: z.string(),
  rankingAge: z.string(),
  competitors: z.string(),

  domesticFinaleCount: z.number().optional(),
  foreignFinaleCount: z.number().optional(),

  ranklistRanking: z.number().optional(),
  ranklistPoints: z.number().optional(),

  id: z.number().optional(),
  idt: z.number().optional(),
  age: z.string().optional(),
  class: z.string().optional(),
  points: z.number().optional(),
  partner: z.string().optional(),
  partnerIdt: z.number().optional(),
  time: z.string().optional(),
});

const athleteSchema = z.object({
  idt: z.number(),
  name: z.string(),
  validFor: z.string(),
  age: z.string(),
  sex: z.string(),
  medicalCheckupExpiration: z.string().nullable().optional().prefault(null),
  barcode: z.string(),
  rankingPoints: z.array(rankingPointsSchema),
});

const athletesResponseSchema = z.object({
  collection: z.array(athleteSchema),
});

export type RankingPoints = z.infer<typeof rankingPointsSchema>;

export type Athlete = z.infer<typeof athleteSchema>;

export interface AthletesResponse {
  collection: Athlete[];
}

export interface FetchAthletesOptions {
  init?: Parameters<typeof fetch>[1];
}

export function parseAthletesResponse(payload: unknown): AthletesResponse {
  try {
    return athletesResponseSchema.parse(payload);
  } catch (parseError) {
    throw new Error('Failed to validate API response ' + JSON.stringify(payload), {
      cause: parseError,
    });
  }
}

export async function fetchAthletesByIdt(
  idt: number,
  options: FetchAthletesOptions = {},
): Promise<AthletesResponse> {
  if (!Number.isInteger(idt)) {
    throw new TypeError('The athlete IDT must be an integer.');
  }

  if (idt < MIN_IDT || idt > MAX_IDT) {
    throw new RangeError(`The athlete IDT must be between ${MIN_IDT} and ${MAX_IDT}.`);
  }

  const response = await fetch(`https://www.csts.cz/api/1/athletes/${idt}`, options.init);

  if (!response.ok) {
    throw new Error(
      `Failed to fetch athlete ${idt}: ${response.status} ${response.statusText}`,
    );
  }

  const payload = await response.json();
  return parseAthletesResponse(payload);
}
