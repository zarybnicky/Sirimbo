import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import { getFederatedCategoryId } from './federatedCategory.ts';
import { upsertFrontiers } from './crawler.queries.ts';
import {
  type competition_type,
  type competitor_type,
  upsertCompetitions,
  upsertEvents,
} from './federated.queries.ts';

const competitionType = [
  'GENERIC OPEN',
  'OPEN',
  'INTERNATIONAL OPEN',
  'WORLD OPEN',
  'WORLD OPEN NS',
  'GRAND SLAM',
  'NATIONAL CHAMPIONSHIP',
  'WORLD CUP',
  'WORLD CUP OTHER',
  'WORLD CHAL CUP',
  'WORLD CHAMPIONSHIP',
  'SEA GAMES',
  'SEA CHAMPIONSHIP',
  'WORLD MASTER GAMES',
  'BALK CHAMPIONSHIP',
  'ASIA CUP',
  'ASIA CHAMPIONSHIP',
  'ASIA PACIFIC CHAMPIONSHIP',
  'EUROPE CUP',
  'EUROPE UNI',
  'EUROPE MASTER',
  'EUROPE CHAMPIONSHIP',
  'EUROPE OPEN CHAMPIONSHIP',
  'EUROPE NORTH CHAMPIONSHIP',
  'EUROPE EAST CHAMPIONSHIP',
  'EUROPE CENTRAL CHAMPIONSHIP',
  'SOUTH EUROPEAN CHAMPIONSHIP',
  'EU CHAMPIONSHIP',
  'SUB CHAMPIONSHIP',
  'CONTINENTAL CHAMPIONSHIP',
  'NORTH AMERICA CHAMPIONSHIP',
  'SOUTH AMERICAN CHAMPIONSHIP',
  'INT TEAM MATCH',
  'PD TNS',
  'PD OPEN',
  'PD SUPER GRAND PRIX',
  'PD EUROPEAN CUP',
  'PD EUROPEAN CHAMPIONSHIP',
  'PD ASIAN CHAMPIONSHIP',
  'PD WORLD OPEN',
  'PD WORLD CUP',
  'PD WORLD CHAMPIONSHIP',
  'DSE NT CH',
  'DSE U CHAMPIONSHIP',
  'DSE CHILDREN GP',
  'DSE CHILDREN T CH',
  'ST OPEN',
  'ST WORLD CHAMPIONSHIP',
  'ST EUROPE CHAMPIONSHIP',
  'SMOOTH WORLD CHAMPIONSHIP',
  'OPEN CARRIBEAN',
  'WORLD CHAMPIONSHIP CAR',
  'URBAN CHAL',
  'URBAN CONT',
  'URBAN WCH',
  'URBAN WORLD SER',
  'URBAN CONT CH',
  'URBAN CONT GAMES',
  'URBAN INT SER',
  'URBAN NATIONAL',
  'URBAN OPEN',
  'URBAN YOG',
  'URBAN WUG',
  'URBAN OQS',
  'HIPHOP EURO CHAMPIONSHIP',
  'HIPHOP WORLD CHAMPIONSHIP',
  'DS OPEN',
  'DS FESTIVAL CH',
  'DS EUROPE CHAMPIONSHIP',
  'DS WORLD CHAMPIONSHIP',
  'IWGA WG',
  'WRRC EURO CHAMPIONSHIP',
  'WRRC WORLD CUP',
  'WDSG',
  'SMOOTH OPEN',
  'CLOSED',
  'GAMES QUALIFIER',
  'WORLD DANCE LEAGUE',
  'OLYMP',
  'CUSTOM',
  'UNKNOWN',
] as const;

const mapCompetitionType = (type: (typeof competitionType)[number]): competition_type => {
  switch (type) {
    case 'OPEN':
    case 'INTERNATIONAL OPEN':
    case 'WORLD OPEN':
    case 'WORLD OPEN NS':
      return 'ranking';
    default:
      if (type.includes('CUP')) return 'cup';
      if (
        type.includes('CHAMPIONSHIP') ||
        type === 'DSE NT CH' ||
        type === 'DSE CHILDREN T CH' ||
        type === 'URBAN WCH' ||
        type === 'URBAN CONT CH' ||
        type === 'DS FESTIVAL CH'
      ) {
        return 'championship';
      }
      return 'unknown';
  }
};

const mapCompetitorType = (
  c: Pick<z.infer<typeof schema>, 'danceform' | 'discipline'>,
): competitor_type => {
  switch (c.danceform) {
    case 'M':
    case 'F':
    case '1 M':
    case '1 F':
    case 'SOLO':
    case '1 VS 1':
    case 'F VS F':
    case 'M VS M':
      return 'solo';
    case 'DUO':
    case 'DUO F':
    case 'DUO M':
    case 'DUO MIX':
    case '2 VS 2':
      return 'duo';
    case 'TRIO X':
    case 'TRIO F':
    case '3 VS 3':
      return 'trio';
    case 'FORMATION':
      return 'formation';
    case 'SMALL TEAM':
    case 'LARGE TEAM':
    case 'BIG TEAM':
    case 'MEGA TEAM':
    case 'T VS T':
    case '5 VS 5':
    case '6 VS 6':
      return 'team';
    case '':
      break;
  }
  if (c.discipline.includes('SOLO')) return 'solo';
  if (c.discipline.includes('FORMATION')) return 'formation';
  if (c.discipline.includes('TEAM')) return 'team';
  return 'couple';
};

const schema = z.object({
  link: z.array(
    z.object({
      href: z.string(),
      rel: z.string(),
      type: z.string().optional(),
    }),
  ),
  id: z.number(),
  date: z.iso.datetime({ local: true }),
  location: z.string(),
  country: z.string(),
  status: z.enum([
    'Closed',
    'Canceled',
    'RegistrationClosed',
    'PreRegistration',
    'Registering',
    'InProgress',
    'Processing',
    'PostponedTBC',
    'Postponed',
  ]),
  type: z.enum(competitionType, { error: '' }),
  age: z.enum([
    'JUVENILE',
    'JUVENILE T',
    'JUVENILE I',
    'JUVENILE II',
    'JUNIOR',
    'JUNIOR T',
    'JUNIOR I',
    'JUNIOR II',
    'UNDER 21',
    'OVER 35',
    'YOUTH',
    'RISING STAR',
    'ADULT',
    'ADULT T',
    'ADULT O',
    'MASTER CLASS',
    'MASTER CLASS I',
    'MASTER CLASS II',
    'MASTER CLASS III',
    'SENIOR',
    'SENIOR T',
    'SENIOR O',
    'SENIOR I',
    'SENIOR II',
    'SENIOR III',
    'SENIOR IV',
    'SENIOR V',
    'MIX',
  ]).or(z.stringFormat('custom-age', /CUSTOM \(.+\)/)),
  division: z.enum([
    'General',
    'Professional',
    'Caribbean',
    'Breaking',
    'Disco',
    'Smooth',
    'HipHop',
    'WRRC',
    'Stage',
    'Any',
  ]),
  danceform: z.enum([
    '',
    'M',
    'F',
    '1 M',
    '1 F',
    'SOLO',
    'DUO',
    'DUO F',
    'DUO M',
    'DUO MIX',
    'TRIO X',
    'TRIO F',
    'SMALL TEAM',
    'LARGE TEAM',
    'BIG TEAM',
    'MEGA TEAM',
    'FORMATION',
    'T VS T',
    '1 VS 1',
    '2 VS 2',
    '3 VS 3',
    '5 VS 5',
    '6 VS 6',
    'F VS F',
    'M VS M',
  ]),
  discipline: z.preprocess(x => typeof x === 'string' ? x.toUpperCase() : x, z.enum([
    'LATIN',
    '3D LATIN',
    'PD LATIN',
    'SYN LATIN',
    'SOLO LATIN',
    'CHOR LATIN',
    'FORMATION LATIN',
    'STANDARD',
    '3D STANDARD',
    'PD STANDARD',
    'SYN STANDARD',
    'SOLO STANDARD',
    'CHOR STANDARD',
    'FORMATION STANDARD',
    '6 DANCE',
    '8 DANCE',
    'TEN DANCE',
    'PD TEN DANCE',
    'PD SHOW DANCE STANDARD',
    'PD SHOW DANCE LATIN',
    'SHOW DANCE STANDARD',
    'SHOW DANCE LATIN',
    'SHOW DANCE STAGE',
    'SHOW DANCE ART',
    'SALSA SHINE',
    'SALSA ONE',
    'SALSA TWO',
    'BACHATA',
    'BACHATA SHINE',
    'BREAK',
    'HIPHOP',
    'HH CHOREO',
    'ACRO DISCO',
    'DISCO',
    'ROCK N ROLL',
    'SMOOTH',
    'PD SMOOTH',
    'RHYTHM',
    'WHEELCHAIR',
    'TAIKE TEAM',
    'JAZZ',
    'BOOGIE WOOGIE',
    'LINEDANCE',
    'CHEERLEADING',
    'MODERN PERFOMING DANCE',
    'SAMBA',
    'CHA CHA CHA',
    'RUMBA',
    'PASO DOBLE',
    'JIVE',
    'WALTZ',
    'TANGO',
    'VIENNESE WALTZ',
    'SLOW FOXTROT',
    'QUICKSTEP',
    'SOLO SAMBA',
    'SOLO CHA CHA CHA',
    'SOLO RUMBA',
    'SOLO PASO DOBLE',
    'SOLO JIVE',
    'SOLO WALTZ',
    'SOLO TANGO',
    'SOLO VIENNESE WALTZ',
    'SOLO SLOW FOXTROT',
    'SOLO QUICKSTEP',
  ]).or(z.stringFormat('custom-discipline', /CUSTOM \(.+\)/))),
  coefficient: z.number().optional(),
  lastmodifiedDate: z.iso.datetime({ local: true }),
  eventId: z.number(),
  groupId: z.number(),
});

export const wdsfCompetition: JsonLoader<z.infer<typeof schema>> = {
  mode: 'json',
  schema: schema,
  buildRequest: (key) => ({
    url: new URL(`https://services.worlddancesport.org/api/1/competition/${key}`),
    init: {
      headers: {
        Authorization: process.env.WDSF_AUTH ?? '',
        Accept: 'application/json',
      },
    },
  }),
  revalidatePeriod: '30d',
  load: async (client, c) => {
    // TODO(wdsf): Persist parsed status, coefficient, eventId, lastmodifiedDate,
    // and link relations, or remove them from the schema.
    const competitorType = mapCompetitorType(c);
    const categoryId = await getFederatedCategoryId({
      series: c.division,
      discipline: c.discipline,
      ageGroup: c.age,
      genderGroup: c.danceform,
      class: c.type,
      competitorType,
    });
    await upsertEvents.run(
      {
        federation: ['wdsf'],
        externalId: [c.groupId.toString()],
        startDate: [c.date],
        endDate: [c.date],
        location: [c.location.trim()],
        country: [c.country.trim()],
        name: [[c.location.trim(), c.country.trim()].filter(Boolean).join(', ')],
      },
      client,
    );
    await upsertCompetitions.run(
      {
        federation: 'wdsf',
        eventExternalId: c.groupId.toString(),
        externalId: [c.id.toString()],
        categoryId: [categoryId],
        competitionType: [mapCompetitionType(c.type)],
        startDate: [c.date],
        endDate: [c.date],
      },
      client,
    );
    await upsertFrontiers.run(
      {
        federations: ['wdsf', 'wdsf'],
        kinds: ['participantIndex', 'officialIndex'],
        keys: [c.id.toString(), c.id.toString()],
      },
      client,
    );
  },
};
