import { z } from 'zod';

import { MAX_IDT, MIN_IDT } from './idts.js';

type FetchFunction = (
  input: Parameters<typeof fetch>[0],
  init?: Parameters<typeof fetch>[1],
) => ReturnType<typeof fetch>;

const defaultFetch: FetchFunction | undefined =
  typeof fetch === 'function' ? fetch.bind(globalThis) : undefined;

const rankingPointsSchema = z.object({
  rankingPointsAge: z.string(),
  idt: z.number().finite(),
  partner: z.string(),
  partnerIdt: z.number().finite(),
  medicalCheckupExpiration: z.string().nullable(),
  personalClass: z.string(),
  personalPoints: z.number().finite(),
  personalDomesticFinaleCount: z.number().finite(),
  personalForeignFinaleCount: z.number().finite(),
  personalApproved: z.boolean(),
  ranklistRanking: z.number().finite(),
  ranklistPoints: z.number().finite(),
  id: z.number().finite(),
  competitorId: z.number().finite(),
  age: z.string(),
  series: z.string(),
  discipline: z.string(),
  rankingAge: z.string(),
  competitors: z.string(),
  class: z.string(),
  points: z.number().finite(),
  domesticFinaleCount: z.number().finite(),
  foreignFinaleCount: z.number().finite(),
  time: z.string(),
});

const athleteSchema = z.object({
  idt: z.number().finite(),
  name: z.string(),
  validFor: z.string(),
  age: z.string(),
  sex: z.string(),
  medicalCheckupExpiration: z.string().nullable(),
  barcode: z.string(),
  rankingPoints: z.array(rankingPointsSchema),
  stt: rankingPointsSchema,
  lat: rankingPointsSchema,
});

const athletesResponseSchema = z.object({
  collection: z.array(athleteSchema).nullish(),
});

export type RankingPoints = z.infer<typeof rankingPointsSchema>;

export type Athlete = z.infer<typeof athleteSchema>;

export interface AthletesResponse {
  collection: Athlete[];
}

export interface FetchAthletesOptions {
  init?: Parameters<typeof fetch>[1];
  fetch?: FetchFunction;
}

const API_ROOT = 'https://www.csts.cz/api/1';

export function parseAthletesResponse(payload: unknown): AthletesResponse {
  const { collection } = athletesResponseSchema.parse(payload);

  return { collection: collection ?? [] };
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

  const fetchImpl = options.fetch ?? defaultFetch;
  if (!fetchImpl) {
    throw new Error('No fetch implementation is available.');
  }

  const response = await fetchImpl(`${API_ROOT}/athletes/${idt}`, options.init);

  if (!response.ok) {
    throw new Error(`Failed to fetch athlete ${idt}: ${response.status} ${response.statusText}`);
  }

  const payload = await response.json();
  return parseAthletesResponse(payload);
}
