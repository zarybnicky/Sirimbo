import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import {
  type AgeGroup,
  ageGroup,
  type CompetitionType,
  competitionType,
  competitorType,
  cstsCompetitionClass,
  disciplineType,
  type DisciplineType,
  mapCompetitorType,
  mapGenderGroup,
  mapOfficialRole,
  numberAsEnum,
  seriesType,
} from './cstsEnums.ts';
import { upsertFrontierKeys } from './crawler.queries.ts';
import {
  ensurePeople,
  mergeEventOfficials,
  type competition_type,
  type gender,
  type official_role,
  upsertCompetitions,
  upsertEvents,
} from './federated.queries.ts';
import { type CategoryParams, getFederatedCategoryIds } from './federatedCategory.ts';
import { makePgtypedCollection } from './pgtypedCollection.ts';
import { danceCode } from './danceProgram.ts';

const competitionSchema = z.object({
  competitionId: z.number(),
  age: z
    .string()
    .transform((x) => x.replace(' ', '_'))
    .refine((x) => Object.keys(ageGroup).includes(x))
    .transform((x) => x as AgeGroup),
  chairPersonId: z.number(),
  checkInEnd: z.iso.time().optional(),
  completedAt: z.iso.datetime({ offset: true }).optional(),
  class: cstsCompetitionClass.optional(),
  toClass: cstsCompetitionClass.optional(),
  competitors: numberAsEnum(competitorType),
  danceDisciplines: z.array(z.string()),
  dances: z.array(danceCode),
  date: z.iso.date(),
  discipline: z
    .string()
    .transform((x) => x.replace('+', '_'))
    .transform((x) => (x === 'TenDances' ? 'TenDance' : x))
    .refine((x) => Object.keys(disciplineType).includes(x))
    .transform((x) => x as DisciplineType),
  grade: numberAsEnum(competitionType),
  excused: z.number(),
  registered: z.number(),
  registrationFee: z.number(),
  series: numberAsEnum(seriesType),
});

const officialSchema = z.object({
  id: z.number(),
  name: z.string(),
  surname: z.string(),
  firstName: z.string(),
  familyName: z.string(),
  country: z.string(),
  licences: z.array(
    z.object({
      grade: z.enum(['-S', '-A', '-B', '-D']),
      discipline: z
        .union([z.enum(['Standard', 'Latin']), z.array(z.enum(['Standard', 'Latin']))])
        .optional(),
      type: z.enum(['Adj', 'Inv', 'ChP', 'Scr', 'LScr']),
      role: z.number(),
    }),
  ),
});

const eventSchema = z.object({
  competitions: z.array(competitionSchema),
  dateFrom: z.iso.date(),
  dateTo: z.iso.date(),
  eventId: z.number(),
  eventTitle: z.string(),
  invigilatorReport: z.string().optional(),
  invigilatorReportPublicationApproved: z.boolean().optional(),
  location: z.string(),
  officials: z.array(officialSchema),
  organizer: z.string().optional(),
  promoter: z.string(),
  registrationState: z.enum(['Closed', 'Open']),
  resultsCanBeSentBefore: z.iso.datetime({ offset: true }),
});

const responseSchema = z.object({
  entity: eventSchema,
});

type Response = z.output<typeof responseSchema>;

export const cstsEvent: JsonLoader<Response> = {
  mode: 'json',
  schema: responseSchema,
  revalidatePeriod: '1 day',
  buildRequest: (key) => {
    return {
      url: new URL(`https://www.csts.cz/api/1/competition_events/${key}`),
      init: {
        headers: {
          Referer: 'https://www.csts.cz/dancesport/kalendar_akci',
        },
      },
    };
  },
  mapResponseToStatus({ httpStatus }) {
    if (httpStatus === 404 || httpStatus === 410) return 'gone';
    return undefined;
  },
  async load(client, { entity: event }) {
    await upsertEvents.run(
      {
        federation: ['csts'],
        externalId: [event.eventId.toString()],
        name: [event.eventTitle],
        startDate: [event.dateFrom],
        endDate: [event.dateTo],
        location: [event.location],
        city: [event.location],
        country: ['Czechia'],
        organizingClubId: [''],
      },
      client,
    );

    const categoryParams: CategoryParams[] = event.competitions.map((competition) => ({
      class:
        !competition.class || competition.class === 'Unknown'
          ? ''
          : competition.toClass &&
              competition.toClass !== competition.class &&
              competition.toClass !== 'Unknown'
            ? `${competition.class}-${competition.toClass}`
            : competition.class,
      ageGroup: competition.age,
      genderGroup: mapGenderGroup(competition.competitors),
      discipline: competition.discipline,
      series: competition.series,
      competitorType: mapCompetitorType(competition.competitors),
    }));
    const categoryIds = await getFederatedCategoryIds(client, categoryParams);
    const competitions = makePgtypedCollection<{
      externalId: string;
      categoryId: string;
      startDate: string;
      endDate: string;
      participantsTotal: number;
      checkInEnd: string;
      completedAt: string;
      registrationFee: string;
      excusedTotal: number;
      competitionType: competition_type;
    }>([
      'externalId',
      'categoryId',
      'startDate',
      'endDate',
      'participantsTotal',
      'checkInEnd',
      'completedAt',
      'registrationFee',
      'excusedTotal',
      'competitionType',
    ]);

    for (const [index, competition] of event.competitions.entries()) {
      const categoryId = categoryIds[index];
      if (!categoryId) continue;
      competitions.add({
        externalId: competition.competitionId.toString(),
        categoryId,
        startDate: competition.date,
        endDate: competition.date,
        participantsTotal: competition.registered,
        checkInEnd: competition.checkInEnd ?? '',
        completedAt: competition.completedAt ?? '',
        registrationFee: competition.registrationFee.toString(),
        excusedTotal: competition.excused,
        competitionType: mapCompetitionType(competition.grade),
      });
    }
    if (competitions.length) {
      await upsertCompetitions.run(
        {
          federation: 'csts',
          eventExternalId: event.eventId.toString(),
          ...competitions.params,
        },
        client,
      );
    }

    const people = makePgtypedCollection<{
      federation: string;
      externalId: string;
      canonicalName: string;
      gender: gender;
    }>(
      ['federation', 'externalId', 'canonicalName', 'gender'],
      ['federation', 'externalId'],
    );
    const eventOfficials = makePgtypedCollection<{
      personId: string;
      role: official_role;
      discipline: string;
      grade: string;
    }>(['personId', 'role', 'discipline', 'grade'], ['personId', 'role', 'discipline']);

    for (const official of event.officials) {
      const personId = `csts:${official.id}`;
      people.add({
        federation: 'csts',
        externalId: official.id.toString(),
        canonicalName: [official.name, official.surname].filter(Boolean).join(' '),
        gender: 'unknown',
      });
      for (const licence of official.licences) {
        const role = mapOfficialRole(licence.type);
        for (const discipline of officialDisciplines(licence.discipline)) {
          eventOfficials.add({ personId, role, discipline, grade: licence.grade });
        }
      }
    }
    if (people.length) await ensurePeople.run(people.params, client);
    await mergeEventOfficials.run(
      { federation: 'csts', eventExternalId: event.eventId.toString(), ...eventOfficials.params },
      client,
    );

    await upsertFrontierKeys.run(
      { federation: 'csts', kind: 'eventCompetitors', keys: [event.eventId.toString()] },
      client,
    );

    const keys = event.competitions
      .filter((x) => !!x.completedAt)
      .map((x) => x.competitionId.toString());
    await upsertFrontierKeys.run(
      { federation: 'csts', kind: 'competitionResults', keys },
      client,
    );
  },
};

function officialDisciplines(discipline?: 'Standard' | 'Latin' | ('Standard' | 'Latin')[]) {
  return discipline == null ? [''] : Array.isArray(discipline) ? discipline : [discipline];
}

function mapCompetitionType(type: CompetitionType): competition_type {
  switch (type) {
    case 'Unknown':
      return 'unknown';
    case 'Cup':
      return 'cup';
    case 'Ranking':
      return 'ranking';
    case 'League':
      return 'league';
    case 'Championship':
      return 'championship';
    case 'TopLevel':
      return 'top_level';
    case 'SuperLeague':
      return 'super_league';
    case 'GCup':
      return 'g_cup';
  }
}
