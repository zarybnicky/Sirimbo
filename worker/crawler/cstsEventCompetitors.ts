import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import { competitorType, mapCompetitorType, numberAsEnum } from './cstsEnums.ts';
import {
  type competitor_role,
  type competitor_type,
  ensureCompetitorsWithComponents,
  type gender,
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
    const components = makePgtypedCollection<PayloadCompetitorComponent>(
      [
        'componentCompetitorId',
        'personId',
        'personFederation',
        'personExternalId',
        'personCanonicalName',
        'personGender',
        'componentRole',
      ],
      ['componentCompetitorId', 'personId'],
    );

    for (const competitor of parsed.collection) {
      const competitorType = mapCompetitorType(competitor.type);
      competitors.add({
        federation: 'csts',
        externalId: competitor.competitorId.toString(),
        label: competitorLabel(competitor),
        type: competitorType,
      });

      entries.add({
        competitionExternalId: competitor.competitionId.toString(),
        competitorId: `csts:${competitor.competitorId}`,
        cancelled: competitor.registrationState !== 1,
      });

      components.add(...competitorComponents(competitor, competitorType));
    }

    if (competitors.length) {
      await ensureCompetitorsWithComponents.run(
        { ...competitors.params, ...components.params },
        client,
      );
    }

    await mergeCompetitionEntriesByEventId.run(
      { federation: 'csts', eventId: String(eventId), ...entries.params },
      client,
    );
  },
};

type EventCompetitor = Response['collection'][number];
type PayloadCompetitorComponent = {
  componentCompetitorId: string;
  personId: string;
  personFederation: string;
  personExternalId: string;
  personCanonicalName: string;
  personGender: gender;
  componentRole: competitor_role;
};

function fullName(...parts: Array<string | undefined>) {
  return parts
    .map((part) => part?.trim())
    .filter(Boolean)
    .join(' ');
}

function competitorLabel(competitor: EventCompetitor) {
  const label = competitor.name?.trim();
  if (label) return label;

  const coupleLabels = competitor.couplesOrDuos?.map((couple) =>
    [fullName(couple.name1, couple.surname1), fullName(couple.name2, couple.surname2)].join(
      ' - ',
    ),
  );
  if (coupleLabels?.length) return coupleLabels.join(', ');

  const personLabels = competitor.persons?.map((person) =>
    fullName(person.name, person.surname),
  );
  return personLabels?.join(', ') ?? '';
}

function competitorComponents(competitor: EventCompetitor, type: competitor_type) {
  const competitorId = `csts:${competitor.competitorId}`;
  const components: PayloadCompetitorComponent[] = [];

  for (const couple of competitor.couplesOrDuos ?? []) {
    if (type === 'couple') {
      components.push(
        component(competitorId, couple.idt1, fullName(couple.name1, couple.surname1), 'lead'),
        component(
          competitorId,
          couple.idt2,
          fullName(couple.name2, couple.surname2),
          'follow',
        ),
      );
    } else {
      components.push(
        component(
          competitorId,
          couple.idt1,
          fullName(couple.name1, couple.surname1),
          'member',
        ),
        component(
          competitorId,
          couple.idt2,
          fullName(couple.name2, couple.surname2),
          'member',
        ),
      );
    }
  }

  for (const person of competitor.persons ?? []) {
    components.push(
      component(
        competitorId,
        person.idt,
        fullName(person.name, person.surname),
        person.backup ? 'substitute' : 'member',
      ),
    );
  }

  return components;
}

function component(
  competitorId: string,
  personExternalId: number,
  personCanonicalName: string,
  role: competitor_role,
): PayloadCompetitorComponent {
  return {
    componentCompetitorId: competitorId,
    personId: `csts:${personExternalId}`,
    personFederation: 'csts',
    personExternalId: String(personExternalId),
    personCanonicalName,
    personGender: 'unknown',
    componentRole: role,
  };
}
