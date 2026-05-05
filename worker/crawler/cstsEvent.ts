import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import { type AgeGroup, ageGroup, competitionClassType, competitionType, competitorType, disciplineType, numberAsEnum, seriesType, type DisciplineType } from './cstsEnums.ts';
import { upsertFrontierKeys } from './crawler.queries.ts';

const competitionSchema = z.object({
  competitionId: z.number(),
  age: z.string()
    .transform(x => x.replace(' ', '_'))
    .refine((x) => Object.keys(ageGroup).includes(x))
    .transform((x) => x as AgeGroup),
  chairPersonId: z.number(),
  checkInEnd: z.iso.time().optional(),
  completedAt: z.iso.datetime({ offset: true }).optional(),
  class: numberAsEnum(competitionClassType).optional(),
  competitors: numberAsEnum(competitorType),
  danceDisciplines: z.array(z.string()),
  dances: z.array(z.string()),
  date: z.iso.date(),
  discipline: z.string()
    .transform(x => x.replace('+', '_'))
    .transform(x => x === 'TenDances' ? 'TenDance' : x)
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
  licences: z.array(z.object({
    grade: z.enum(['-S', '-A', '-B', '-D']),
    discipline: z.union([
      z.enum(['Standard', 'Latin']),
      z.array(z.enum(['Standard', 'Latin'])),
    ]).optional(),
    type: z.enum(['Adj', 'Inv', 'ChP', 'Scr']),
    role: z.number(),
  })),
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
  async load(client, { entity: event }) {
    for (const competition of event.competitions) {
      // category same as in member
    }
    const keys = event.competitions.filter(x => !!x.completedAt).map(x => x.competitionId.toString());
    await upsertFrontierKeys.run(
      { federation: 'csts', kind: 'competitionResults', keys },
      client,
    );
  },
};
