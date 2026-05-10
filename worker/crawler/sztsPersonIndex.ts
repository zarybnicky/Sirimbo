import * as cheerio from 'cheerio';
import {
  ensurePeople,
  mergePersonLicenses,
  type person_license_discipline,
  type person_license_kind,
  type person_license_status,
} from './federated.queries.ts';
import { makePgtypedCollection } from './pgtypedCollection.ts';
import type { HtmlLoader } from './types.ts';

export const sztsMemberIndex = sztsPersonIndexLoader('https://szts.ksis.eu/menu.php?akcia=CZ');
export const sztsTrainerIndex = sztsLicenseIndexLoader(
  'https://szts.ksis.eu/menu.php?akcia=CZT',
  'trainerIndex',
  [{ kind: 'trainer', gradeColumn: 5, statusColumn: 6 }],
);
export const sztsOfficialIndex = sztsLicenseIndexLoader(
  'https://szts.ksis.eu/menu.php?akcia=CZF',
  'officialIndex',
  [{ kind: 'official', statusColumn: 5 }],
);
export const sztsScrutineerIndex = sztsLicenseIndexLoader(
  'https://szts.ksis.eu/menu.php?akcia=CZS',
  'scrutineerIndex',
  [{ kind: 'scrutineer', statusColumn: 5 }],
);
export const sztsJudgeIndex = sztsLicenseIndexLoader(
  'https://szts.ksis.eu/menu.php?akcia=CZR',
  'judgeIndex',
  [{ kind: 'adjudicator', gradeColumn: 5, statusColumn: 7 }],
);

function sztsPersonIndexLoader(url: string): HtmlLoader {
  return {
    mode: 'text',
    revalidatePeriod: '1 day',
    buildRequest: () => ({ url: new URL(url) }),
    async load(client, html) {
      const people = makePgtypedCollection<{
        federation: string;
        externalId: string;
        canonicalName: string;
        gender: 'unknown';
      }>(['federation', 'externalId', 'canonicalName', 'gender'], ['federation', 'externalId']);

      for (const person of parseSztsPersonIndex(html)) {
        people.add({
          federation: 'szts',
          externalId: person.externalId,
          canonicalName: person.canonicalName,
          gender: 'unknown',
        });
      }

      if (people.length) await ensurePeople.run(people.params, client);
    },
  };
}

function parseSztsPersonIndex(html: string) {
  const people: Array<{ externalId: string; canonicalName: string }> = [];
  const $ = cheerio.load(html);

  $('table tr').each((_, row) => {
    const cells = $(row).find('td');
    const externalId = cells.eq(0).text().trim();
    const canonicalName = cells.eq(1).text().trim().replace(/\s+/g, ' ');

    if (/^\d+$/.test(externalId) && canonicalName) people.push({ externalId, canonicalName });
  });

  return people;
}

type SztsLicenseColumns = {
  kind: person_license_kind;
  gradeColumn?: number;
  statusColumn?: number;
};

function sztsLicenseIndexLoader(
  url: string,
  sourceKind: string,
  licenseColumns: SztsLicenseColumns[],
): HtmlLoader {
  return {
    mode: 'text',
    revalidatePeriod: '1 day',
    buildRequest: () => ({ url: new URL(url) }),
    async load(client, html) {
      const parsed = parseSztsLicenseIndex(html, licenseColumns);
      if (parsed.people === 0) {
        throw new Error(`Expected SZTŠ ${sourceKind} to contain person rows`);
      }
      if (parsed.licenses.length === 0) {
        throw new Error(`Expected SZTŠ ${sourceKind} to contain license rows`);
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
          discipline: 'general',
          grade: license.grade,
          validUntil: '',
          status: license.status,
        });
      }

      await mergePersonLicenses.run(
        {
          scopeFederation: 'szts',
          scopePersonId: [],
          managedKind: licenseColumns.map((columns) => columns.kind),
          managedDiscipline: licenseColumns.map(() => 'general' as person_license_discipline),
          ...licenses.params,
        },
        client,
      );
    },
  };
}

function parseSztsLicenseIndex(html: string, licenseColumns: SztsLicenseColumns[]) {
  const licenses: Array<{
    externalId: string;
    canonicalName: string;
    kind: person_license_kind;
    grade: string;
    status: person_license_status;
  }> = [];
  let people = 0;
  const $ = cheerio.load(html);

  $('table tr').each((_, row) => {
    const cells = $(row)
      .find('td')
      .toArray()
      .map((cell) => $(cell).text().trim().replace(/\s+/g, ' '));
    const externalId = cells[0] ?? '';
    const canonicalName = cells[1] ?? '';

    if (!/^\d+$/.test(externalId) || !canonicalName) return;
    people++;

    for (const columns of licenseColumns) {
      const grade = columnValue(cells, columns.gradeColumn);
      if (columns.gradeColumn != null && !grade) continue;

      const statusText = columnValue(cells, columns.statusColumn);
      licenses.push({
        externalId,
        canonicalName,
        kind: columns.kind,
        grade,
        status: sztsLicenseStatus(statusText),
      });
    }
  });

  return { licenses, people };
}

function columnValue(cells: string[], index?: number) {
  return index == null ? '' : (cells[index] ?? '');
}

function sztsLicenseStatus(value: string): person_license_status {
  const normalized = value.trim().toLocaleLowerCase('sk-SK');
  if (!normalized) return 'unknown';
  if (normalized === 'aktívny') return 'active';
  throw new Error(`Unexpected SZTŠ license status: ${value}`);
}
