import { z } from 'zod';
import type { JsonLoader } from './types.ts';

const roundSchema = z.object({
  round: z.string(),
  dances: z.array(z.string()),
});

const officialSchema = z.object({
  id: z.number().optional(), // idt
  type: z.enum(['ChP', 'Inv', 'Scr', 'LScr', 'Adj']),
  name: z.string(),
  surname: z.string(),
  country: z.string(),
  index: z.number().optional(), // Adj only
  label: z.string().optional(), // Adj only
});

const roundDetailSchema = z.object({
  danceResults: z.array(z.number()),
  marks: z.string(),
  ranking: z.number(),
  rankingTo: z.number(),
  round: z.string(),
  sum: z.number(),
});

const competitorSchema = z.object({
  competitorId: z.number(), // registration ID
  startNumber: z.number(),
  country: z.string(),
  club: z.string().optional(),
  ranking: z.number(),
  rankingTo: z.number(),
  points: z.number().optional(),
  final: z.boolean(),
  completion: z.object({
    completion: z.enum(['normal', 'retirement']),
    lastRound: z.string(),
    lastDance: z.string(),
  }),
  competitor: z.object({
    id: z.number(), // competitor ID
    club: z.string().optional(),
    country: z.string(),
    idt1: z.number().optional(), // empty for teams = only surname1 is the team name/club
    name1: z.string().optional(), // empty for teams
    surname1: z.string(),
    idt2: z.number().optional(),
    name2: z.string().optional(),
    surname2: z.string().optional(),
  }),
  rounds: z.array(roundDetailSchema),
});

const resultSchema = z.object({
  competitionId: z.number(),
  completedAt: z.iso.datetime({ offset: true }),
  type: z.enum(['preliminary']),
  rounds: z.array(roundSchema),
  officials: z.array(officialSchema),
  competitors: z.array(competitorSchema),
});

const responseSchema = z.object({
  entity: resultSchema,
});

type Response = z.output<typeof responseSchema>;

export const cstsCompetitionResults: JsonLoader<Response> = {
  mode: 'json',
  schema: responseSchema,
  revalidatePeriod: '1 day',
  buildRequest: (key) => {
    return {
      url: new URL(`https://www.csts.cz/api/1/competitions/${key}/result`),
      init: {
        headers: {
          Referer: 'https://www.csts.cz/dancesport/kalendar_akci',
        },
      },
    };
  },
  async load(client, parsed) {
  },
};
