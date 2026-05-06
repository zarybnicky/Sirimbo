import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import { competitorType, mapCompetitorType, numberAsEnum } from './cstsEnums.ts';
import {
  type competitor_type,
  ensureCompetitors,
  mergeCompetitionEntriesByEventId,
} from './federated.queries.ts';
import { makePgtypedCollection } from './pgtypedCollection.ts';

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
  collection: z.array(competitorSchema).prefault([]),
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
        headers: {
          Referer: 'https://www.csts.cz/dancesport/kalendar_akci',
        },
      },
    };
  },
  async load(client, parsed) {
    const eventId = parsed.collection[0]?.eventId;
    if (eventId == null) return;
    if (parsed.collection.some((competitor) => competitor.eventId !== eventId)) {
      throw new Error('Expected one eventId in csts event competitors response');
    }

    const competitors = makePgtypedCollection<{
      federation: string;
      externalId: string;
      label: string;
      type: competitor_type;
    }>(['federation', 'externalId', 'label', 'type'], ['federation', 'externalId']);
    const entries = makePgtypedCollection<{
      competitionExternalId: string;
      competitorId: string;
      cancelled: boolean;
    }>(
      ['competitionExternalId', 'competitorId', 'cancelled'],
      ['competitionExternalId', 'competitorId'],
    );

    for (const competitor of parsed.collection) {
      competitors.add({
        federation: 'csts',
        externalId: competitor.competitorId.toString(),
        label: '',
        type: mapCompetitorType(competitor.type),
      });

      entries.add({
        competitionExternalId: competitor.competitionId.toString(),
        competitorId: `csts:${competitor.competitorId}`,
        cancelled: competitor.registrationState !== 1,
      });
    }

    if (competitors.length) await ensureCompetitors.run(competitors.params, client);

    await mergeCompetitionEntriesByEventId.run(
      { federation: 'csts', eventId: String(eventId), ...entries.params },
      client,
    );
  },
};
