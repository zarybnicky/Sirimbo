import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import { numberAsEnum, competitorType } from './cstsEnums.ts';

const competitorSchema = z.object({
  competitorId: z.number(),
  eventId: z.number(),
  competitionId: z.number(),
  registrationState: z.number(),
  registrationDate: z.iso.datetime({ offset: true }),
  deregisteredDate: z.iso.datetime({ offset: true }).optional(),
  startsFromRound: z.number(),
  type: numberAsEnum(competitorType),
  startType: z.number().optional(),
  country: z.string(),
  name: z.string().optional(),
  club: z.string().optional(),

  captain: z.string().optional(),
  points: z.number().optional(),
  classVaue: z.number().optional(),
  finaleCount: z.number().optional(),
  ranklistPosition: z.number().optional(),

  couplesOrDuos: z
    .array(
      z.object({
        competitorId: z.number(),
        idt1: z.number(),
        name1: z.string(),
        surname1: z.string(),
        idt2: z.number(),
        name2: z.string(),
        surname2: z.string(),
        backup: z.boolean().optional(),
      }),
    )
    .nonempty()
    .optional(),
  persons: z
    .array(
      z.object({
        competitorId: z.number(),
        idt: z.number(),
        name: z.string(),
        surname: z.string(),
        backup: z.boolean().optional(),
      }),
    )
    .nonempty()
    .optional(),
});

const responseSchema = z.object({
  collection: z.array(competitorSchema),
});

type Response = z.output<typeof responseSchema>;

export const cstsEventCompetitors: JsonLoader<Response> = {
  mode: 'json',
  schema: responseSchema,
  revalidatePeriod: '1 day',
  buildRequest: (key) => {
    return {
      url: new URL(`https://www.csts.cz/api/1/events/${key}/competitors`),
      init: {
        referrer: 'https://www.csts.cz/dancesport/kalendar_akci',
      },
    };
  },
  async load(client, _frontier, parsed) {
    for (const competitor of parsed.collection) {
      // Check on enums - type, reg.state
      // ensureCompetitor (bulk)
      // ensureCompetition (only fed+id + fed+eventId)
    }
  },
};
