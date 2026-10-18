import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import {
  type competitor_type,
  ensureCompetitors,
  getCompetitionContext,
  mergeWdsfCompetitionEntries,
} from './federated.queries.ts';
import { makePgtypedCollection } from './pgtypedCollection.ts';
import { resolveCompetitor } from './wdsfParticipant.ts';

const status = z.enum([
  'Disqualified',
  'Registered',
  'Present',
  'Unknown',
  'Provisional',
  'Excused',
  'Withdrawn',
  'Noshow',
]);

export const schema = z.array(
  z.discriminatedUnion('Kind', [
    z.object({
      Kind: z.literal("Team"),
      link: z.array(
        z.object({ href: z.string(), rel: z.string(), type: z.string() })
      ),
      Team: z.string().optional(),
      TeamCountryXmlName: z.string().optional(),
      id: z.number(),
      number: z.number(),
      status,
      nationalreference: z.string().nullish(),
    }),
    z.object({
      Kind: z.undefined().optional(),
      link: z.array(
        z.object({ href: z.string(), rel: z.string(), type: z.string() })
      ),
      id: z.number(),
      name: z.string(),
      country: z.string().nullable(),
      number: z.number(),
      status,
      nationalreference: z.string().nullish(),
    })
  ]),
)

export const wdsfParticipantIndex: JsonLoader<z.infer<typeof schema>> = {
  mode: 'json',
  schema,
  buildRequest: (key) => ({
    url: new URL(`https://services.worlddancesport.org/api/1/participant?competitionID=${key}`),
    init: {
      headers: {
        Authorization: process.env.WDSF_AUTH ?? '',
        Accept: 'application/json',
      },
    },
  }),
  revalidatePeriod: '7d',
  mapResponseToStatus({ httpStatus }) {
    if (httpStatus === 404) return 'gone';
    if (httpStatus === 500) return 'error';
    return undefined;
  },
  async load(client, parsed, key) {
    const [competition] = await getCompetitionContext.run(
      { federation: 'wdsf', externalId: key },
      client,
    );
    if (!competition) throw new Error(`Competition wdsf:${key} is missing`);

    const competitors = makePgtypedCollection<{
      federation: string;
      externalId: string;
      type: competitor_type;
      label: string;
    }>(['federation', 'externalId', 'type', 'label'], ['federation', 'externalId']);

    const entries = makePgtypedCollection<{
      competitorId: string;
      cancelled: boolean;
    }>(['competitorId', 'cancelled'], ['competitorId']);

    let participantsTotal = 0;
    for (const participant of parsed) {
      const competitionLink = participant.link.find(
        (link) => link.rel === 'http://services.worlddancesport.org/rel/participant/competition',
      );
      if (competitionLink && new URL(competitionLink.href).pathname.split('/').at(-1) !== key) {
        throw new Error(`Participant ${participant.id} does not belong to competition wdsf:${key}`);
      }

      const active = ['Present', 'Registered', 'Provisional'].includes(participant.status);
      if (active) participantsTotal++;

      const competitor = resolveCompetitor(participant);
      if (!competitor) continue;

      competitors.add({
        federation: 'wdsf',
        externalId: competitor.externalId,
        type: competitor.type,
        label: competitor.label,
      });
      entries.add({
        competitorId: `wdsf:${competitor.externalId}`,
        cancelled: ['Excused', 'Noshow', 'Withdrawn'].includes(participant.status),
      });
    }

    if (competitors.length) await ensureCompetitors.run(competitors.params, client);
    await mergeWdsfCompetitionEntries.run(
      {
        competitionId: competition.id,
        participantsTotal,
        excusedTotal: parsed.length - participantsTotal,
        ...entries.params,
      },
      client,
    );

    return {
      upsertFrontier: parsed.flatMap((participant) => {
        const couple = participant.link.find(
          (link) => link.rel === 'http://services.worlddancesport.org/rel/participant/couple',
        );
        return [
          { federation: 'wdsf', kind: 'participant', key: participant.id.toString() },
          ...(couple ? [{
            federation: 'wdsf', kind: 'couple',
            key: new URL(couple.href).pathname.split('/').at(-1)!,
          }] : []),
        ];
      }),
    };
  },
};
