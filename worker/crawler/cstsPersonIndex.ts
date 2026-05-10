import * as cheerio from 'cheerio';
import {
  mergePersonLicenses,
  type person_license_discipline,
  type person_license_kind,
  type person_license_status,
} from './federated.queries.ts';
import { makePgtypedCollection } from './pgtypedCollection.ts';
import type { HtmlLoader } from './types.ts';

export const cstsTrainerIndex = cstsPersonIndexLoader(
  'https://www.csts.cz/cs/Evidence/SeznamTreneru',
  'trainerIndex',
  [{ kind: 'trainer', discipline: 'general', grades: ['I', 'II', 'III'], gradeOffset: 5 }],
);

export const cstsJudgeIndex = cstsPersonIndexLoader(
  'https://www.csts.cz/cs/Evidence/SeznamPorotcu',
  'judgeIndex',
  [
    {
      kind: 'adjudicator',
      discipline: 'standard',
      grades: ['I.M', 'I.A', 'II', 'III'],
      gradeOffset: 5,
    },
    {
      kind: 'adjudicator',
      discipline: 'latin',
      grades: ['I.M', 'I.A', 'II', 'III'],
      gradeOffset: 11,
    },
  ],
);

export const cstsOfficialIndex = cstsPersonIndexLoader(
  'https://www.csts.cz/cs/Evidence/SeznamFunkcionaru',
  'officialIndex',
  [
    { kind: 'chairperson', discipline: 'general', grades: ['I', 'II'], gradeOffset: 5 },
    { kind: 'invigilator', discipline: 'general', grades: ['I', 'II'], gradeOffset: 9 },
    { kind: 'scrutineer', discipline: 'general', grades: ['I', 'II'], gradeOffset: 13 },
  ],
);

type CstsLicenseBlock = {
  kind: person_license_kind;
  discipline: person_license_discipline;
  grades: string[];
  gradeOffset: number;
};

function cstsPersonIndexLoader(
  url: string,
  sourceKind: string,
  licenseBlocks: CstsLicenseBlock[],
): HtmlLoader {
  return {
    mode: 'text',
    revalidatePeriod: '1 day',
    buildRequest: () => ({ url: new URL(url) }),
    async load(client, html) {
      const parsed = parseCstsPersonIndex(html, licenseBlocks);
      if (parsed.people === 0) {
        throw new Error(`Expected ČSTS ${sourceKind} to contain person rows`);
      }
      if (parsed.licenses.length === 0) {
        throw new Error(`Expected ČSTS ${sourceKind} to contain license rows`);
      }

      const licenses = makePgtypedCollection<{
        externalId: string;
        canonicalName: string;
        gender: 'unknown';
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

      for (const license of parsed.licenses) {
        licenses.add({
          externalId: license.externalId,
          canonicalName: license.canonicalName,
          gender: 'unknown',
          kind: license.kind,
          discipline: license.discipline,
          grade: license.grade,
          validUntil: license.validUntil,
          status: 'active',
        });
      }

      await mergePersonLicenses.run(
        {
          scopeFederation: 'csts',
          scopePersonId: [],
          managedKind: licenseBlocks.map((block) => block.kind),
          managedDiscipline: licenseBlocks.map((block) => block.discipline),
          ...licenses.params,
        },
        client,
      );
    },
  };
}

function parseCstsPersonIndex(html: string, licenseBlocks: CstsLicenseBlock[]) {
  const licenses: Array<{
    externalId: string;
    canonicalName: string;
    kind: person_license_kind;
    discipline: person_license_discipline;
    grade: string;
    validUntil: string;
  }> = [];
  let people = 0;
  const $ = cheerio.load(html);

  $('table.tab1 tr').each((_, row) => {
    const cells = $(row).find('td');
    const link = cells.eq(1).find('a[href^="/cs/Clenove/Detail/"]');
    const externalId = /^\/cs\/Clenove\/Detail\/(\d+)$/.exec(link.attr('href') ?? '')?.[1];
    const canonicalName = [cells.eq(2).text(), link.text()]
      .map((part) => part.trim())
      .filter(Boolean)
      .join(' ');

    if (!externalId || !canonicalName) return;
    people++;

    for (const block of licenseBlocks) {
      const grade = block.grades.entries().find(([index, _grade]) => cells.eq(block.gradeOffset + index).find('img[alt="X"]').length)?.[1];
      if (!grade) continue;

      licenses.push({
        externalId,
        canonicalName,
        kind: block.kind,
        discipline: block.discipline,
        grade,
        validUntil: validUntilYear(cells.eq(block.gradeOffset + block.grades.length).text()),
      });
    }
  });

  return { licenses, people };
}

function validUntilYear(value: string) {
  const trimmed = value.trim();
  if (!trimmed) return '';

  const year = /^\d{4}$/.exec(trimmed)?.[0];
  if (!year) throw new Error(`Unexpected ČSTS license validity year: ${trimmed}`);
  return `${year}-12-31`;
}
