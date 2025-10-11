import { z } from 'zod';
import { MAX_IDT, MIN_IDT } from './idts.ts';

const rankingPointsSchema = z.object({
  rankingPointsAge: z.string(),
  medicalCheckupExpiration: z.string().optional().nullable().default(null),
  personalClass: z.string(),
  personalPoints: z.number().finite(),
  personalDomesticFinaleCount: z.number().finite(),
  personalForeignFinaleCount: z.number().finite(),
  personalApproved: z.boolean(),
  competitorId: z.number().finite(),
  series: z.string(),
  discipline: z.string(),
  rankingAge: z.string(),
  competitors: z.string(),

  domesticFinaleCount: z.number().finite().optional(),
  foreignFinaleCount: z.number().finite().optional(),

  ranklistRanking: z.number().finite().optional(),
  ranklistPoints: z.number().finite().optional(),

  id: z.number().finite().optional(),
  idt: z.number().finite().optional(),
  age: z.string().optional(),
  class: z.string().optional(),
  points: z.number().finite().optional(),
  partner: z.string().optional(),
  partnerIdt: z.number().finite().optional(),
  time: z.string().optional(),
});

const athleteSchema = z.object({
  idt: z.number().finite(),
  name: z.string(),
  validFor: z.string(),
  age: z.string(),
  sex: z.string(),
  medicalCheckupExpiration: z.string().nullable().optional().default(null),
  barcode: z.string(),
  rankingPoints: z.array(rankingPointsSchema),
  stt: rankingPointsSchema.optional(),
  lat: rankingPointsSchema.optional(),
});

const athletesResponseSchema = z.object({
  collection: z.array(athleteSchema).default([]),
});

export type RankingPoints = z.infer<typeof rankingPointsSchema>;

export type Athlete = z.infer<typeof athleteSchema>;

export interface AthletesResponse {
  collection: Athlete[];
}

export interface FetchAthletesOptions {
  init?: Parameters<typeof fetch>[1];
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
  try {
    return athletesResponseSchema.parse(payload);
  } catch (parseError) {
    throw new Error('Failed to validate API response ' + JSON.stringify(payload), {
      cause: parseError,
    });
  }
}
