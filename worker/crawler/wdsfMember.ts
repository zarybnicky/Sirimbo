import type { JsonLoader } from './types.ts';
import {
  mergePersonLicenses,
  upsertPeopleDetailed,
  type gender,
  type person_license_discipline,
  type person_license_kind,
  type person_license_status,
} from './federated.queries.ts';
import { makePgtypedCollection } from './pgtypedCollection.ts';
import { z } from 'zod';

const wdsfLicenseTypeSchema = z.enum([
  'Examiner',
  'Chairman',
  'Scrutiny',
  'Athlete',
  'Adjudicator',
  'DJ',
  'HeadJudge',
  'Invigilator',
  '10',
]);

const wdsfLicenseStatusSchema = z.enum([
  'Active',
  'Expired',
  'Revoked',
  'Resting',
  'Retired',
  'Aspiring',
  'Suspended',
]);

const wdsfLicenseDisciplineSchema = z.enum([
  'Stage',
  'Smooth',
  'Disco',
  'SoloSyncroChoreo',
  'General',
  'Standard',
  'Latin',
  'Caribbean',
  'Professional',
  'Breaking',
  'HipHop',
  'Unknown',
]);

const personSchema = z.object({
  link: z.array(
    z.object({ href: z.string(), rel: z.string(), type: z.string().optional() }),
  ),
  id: z.number(),
  nickname: z.string().nullable(),
  name: z.string(),
  surname: z.string().nullable(),
  sex: z.enum(['Male', 'Female', '']),
  nationality: z.string().nullable(),
  country: z.string().nullable(),
  ageGroup: z.string().nullable(),
  yearOfBirth: z.number(),
  nationalReference: z.string().nullable(),
  licenses: z.array(
    z.object({
      type: wdsfLicenseTypeSchema,
      status: wdsfLicenseStatusSchema,
      division: wdsfLicenseDisciplineSchema,
      disciplines: z.array(z.string()),
      grade: z.string().optional(),
      expiresOn: z.iso.date().optional(),
      wrlBLockedUntil: z.iso.date().optional(),
      cupOrChampionshipBlockedUntil: z.iso.date().optional(),
    }),
  ).optional(),
});

type WdsfLicenseType = z.output<typeof wdsfLicenseTypeSchema>;
type WdsfLicenseStatus = z.output<typeof wdsfLicenseStatusSchema>;
type WdsfLicenseDiscipline = z.output<typeof wdsfLicenseDisciplineSchema>;

export const wdsfMember: JsonLoader<z.output<typeof personSchema>> = {
  mode: 'json',
  schema: personSchema,
  revalidatePeriod: '5 day',
  buildRequest: (key) => ({
    url: new URL(`https://services.worlddancesport.org/api/1/person/${key}`),
    init: {
      headers: {
        Authorization: process.env.WDSF_AUTH!,
        Accept: 'application/json',
      },
    },
  }),
  mapResponseToStatus({ httpStatus }) {
    if (httpStatus === 404) return 'gone';
    return undefined;
  },
  async load(client, member) {
    await upsertPeopleDetailed.run(
      {
        federation: ['wdsf'],
        externalId: [member.id.toString()],
        canonicalName: [[member.name, member.surname].join(' ').trim()],
        firstName: [member.name],
        lastName: [member.surname ?? ''],
        gender: [wdsfGender(member.sex)],
        nationality: [member.nationality ?? ''],
        ageGroup: [member.ageGroup ?? ''],
        medicalCheckupExpiration: [''],
      },
      client,
    );

    const licenses = makePgtypedCollection<{
      externalId: string;
      canonicalName: string;
      gender: gender;
      kind: person_license_kind;
      discipline: person_license_discipline;
      grade: string;
      validUntil: string;
      status: person_license_status;
    }>(
      [
        'externalId',
        'canonicalName',
        'gender',
        'kind',
        'discipline',
        'grade',
        'validUntil',
        'status',
      ],
      ['externalId', 'kind', 'discipline'],
    );

    const canonicalName = [member.name, member.surname].join(' ').trim();
    const gender = wdsfGender(member.sex);
    for (const license of member.licenses ?? []) {
      const kind = wdsfLicenseKind(license.type);
      if (!kind) continue;

      const status = wdsfLicenseStatus(license.status);
      const disciplines = license.disciplines.length
        ? license.disciplines.map(parseWdsfLicenseDiscipline)
        : fallbackWdsfLicenseDiscipline(license.division, license.grade ?? '');

      for (const { discipline, grade } of disciplines) {
        licenses.add({
          externalId: member.id.toString(),
          canonicalName,
          gender,
          kind,
          discipline,
          grade,
          validUntil: license.expiresOn ?? '',
          status,
        });
      }
    }

    await mergePersonLicenses.run(
      {
        scopeFederation: 'wdsf',
        scopeSourceKind: 'member',
        scopePersonId: [`wdsf:${member.id}`],
        ...licenses.params,
      },
      client,
    );
  },
};

function wdsfGender(value: z.output<typeof personSchema>['sex']): gender {
  switch (value) {
    case 'Male':
      return 'male';
    case 'Female':
      return 'female';
    case '':
      return 'unknown';
  }
}

function wdsfLicenseKind(value: WdsfLicenseType): person_license_kind | undefined {
  switch (value) {
    case 'Athlete':
      return 'athlete';
    case 'Adjudicator':
      return 'adjudicator';
    case 'Chairman':
      return 'chairperson';
    case 'Scrutiny':
      return 'scrutineer';
    case 'Examiner':
      return 'examiner';
    case 'DJ':
      return 'dj';
    case 'HeadJudge':
      return 'head_judge';
    case 'Invigilator':
      return 'invigilator';
    case '10':
      return undefined;
  }
}

function parseWdsfLicenseDiscipline(value: string) {
  const match = /^(.+) \(([^()]*)\) for .*[ \t]+acquired on ([0-9]{2}\/[0-9]{2}\/[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2})$/.exec(value);
  if (!match) throw new Error(`Unexpected WDSF license discipline: ${value}`);

  return {
    discipline: wdsfLicenseDiscipline(match[1]),
    grade: match[2],
  };
}

function fallbackWdsfLicenseDiscipline(division: WdsfLicenseDiscipline, grade: string) {
  if (division === 'Professional') return [];
  return [{ discipline: wdsfLicenseDiscipline(division), grade }];
}

function wdsfLicenseDiscipline(value: WdsfLicenseDiscipline | string): person_license_discipline {
  switch (value) {
    case 'General':
      return 'general';
    case 'Standard':
      return 'standard';
    case 'Latin':
      return 'latin';
    case 'Breaking':
      return 'breaking';
    case 'HipHop':
      return 'hiphop';
    case 'Caribbean':
      return 'caribbean';
    case 'Stage':
      return 'stage';
    case 'Smooth':
      return 'smooth';
    case 'Disco':
    case 'Disco Dance':
      return 'disco';
    case 'SoloSyncroChoreo':
      return 'solo_syncro_choreo';
    case 'Ten Dance':
      return 'ten_dance';
    case 'Show Dance Standard':
      return 'show_dance_standard';
    case 'Show Dance Latin':
      return 'show_dance_latin';
    case 'Formation Standard':
      return 'formation_standard';
    case 'Formation Latin':
      return 'formation_latin';
    case 'PD Standard':
      return 'pd_standard';
    case 'PD Latin':
      return 'pd_latin';
    case 'PD Ten Dance':
      return 'pd_ten_dance';
    case 'PD Show Dance Standard':
      return 'pd_show_dance_standard';
    case 'PD Show Dance Latin':
      return 'pd_show_dance_latin';
    case 'Caribbean Dances':
      return 'caribbean';
    case 'Hip Hop':
      return 'hiphop';
    case 'Unknown':
      return 'unknown';
    default:
      throw new Error(`Unexpected WDSF license discipline: ${value}`);
  }
}

function wdsfLicenseStatus(value: WdsfLicenseStatus): person_license_status {
  switch (value) {
    case 'Active':
      return 'active';
    case 'Expired':
      return 'expired';
    case 'Revoked':
      return 'revoked';
    case 'Resting':
      return 'resting';
    case 'Retired':
      return 'retired';
    case 'Aspiring':
      return 'aspiring';
    case 'Suspended':
      return 'suspended';
  }
}
