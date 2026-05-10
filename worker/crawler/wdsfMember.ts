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
      disciplines: z.array(wdsfLicenseDisciplineSchema),
      grade: z.string().optional(),
      expiresOn: z.iso.date().optional(),
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
      const status = wdsfLicenseStatus(license.status);
      const disciplines = license.disciplines.length ? license.disciplines : [license.division];

      for (const sourceDiscipline of disciplines) {
        const discipline = wdsfLicenseDiscipline(sourceDiscipline);
        licenses.add({
          externalId: member.id.toString(),
          canonicalName,
          gender,
          kind,
          discipline,
          grade: license.grade ?? '',
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

function wdsfLicenseKind(value: WdsfLicenseType): person_license_kind {
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
  }
}

function wdsfLicenseDiscipline(value: WdsfLicenseDiscipline): person_license_discipline {
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
      return 'disco';
    case 'SoloSyncroChoreo':
      return 'solo_syncro_choreo';
    case 'Professional':
      return 'professional';
    case 'Unknown':
      return 'unknown';
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
