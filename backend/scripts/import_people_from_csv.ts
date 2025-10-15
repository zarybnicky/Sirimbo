#!/usr/bin/env node

import { readFile } from 'node:fs/promises';
import path from 'node:path';
import process from 'node:process';
import pg from 'pg';
import { parse } from 'csv-parse/sync';

type Queryable = pg.Client;

const { Client } = pg;

const DEFAULT_NATIONALITY = 'Czech Republic';
const DEFAULT_COHORT_COLOR = '#808080';

type ImportArgs = {
  file?: string;
  tenantId?: number;
  defaultNationality?: string;
  defaultCohortColor?: string;
  createMissingCohorts: boolean;
  syncMemberships: boolean;
  dryRun: boolean;
  help?: boolean;
};

type PersonInput = {
  first_name: string;
  last_name: string;
  gender?: string;
  email?: string;
  phone?: string;
  nationality?: string;
  birth_date?: string;
  prefix_title?: string;
  suffix_title?: string;
  bio?: string;
  tax_identification_number?: string;
  national_id_number?: string;
  csts_id?: string;
  wdsf_id?: string;
  external_ids?: string[];
};

type PersonField = keyof PersonInput;

type Stats = {
  processed: number;
  createdPeople: number;
  updatedPeople: number;
  createdMemberships: number;
  reactivatedMemberships: number;
  deactivatedMemberships: number;
  syncedMembershipLists: number;
  skippedRows: number;
};

type BuildResult = {
  person: PersonInput;
  providedFields: Set<PersonField>;
  cohortNames: string[];
};

type CsvRecord = Record<string, unknown>;

type FieldAliasKey = PersonField | 'cohorts';

const RAW_PERSON_FIELD_ALIASES: Record<FieldAliasKey, readonly string[]> = {
  first_name: [
    'first_name',
    'firstname',
    'given_name',
    'name',
    'first',
    'křestní jméno',
    'křestní_jméno',
    'krestni jmeno',
    'krestni_jmeno',
    'jméno',
    'jmeno',
  ],
  last_name: [
    'last_name',
    'lastname',
    'surname',
    'family_name',
    'familyname',
    'příjmení',
    'prijmeni',
  ],
  gender: ['gender', 'sex', 'pohlaví', 'pohlavi'],
  email: ['email', 'email_address', 'e-mail', 'e_mail'],
  phone: [
    'phone',
    'phone_number',
    'telephone',
    'telefon',
    'telefonní číslo',
    'telefonní_číslo',
    'telefonni cislo',
    'telefonni_cislo',
  ],
  cohorts: [
    'cohorts',
    'cohort_names',
    'cohort_name',
    'cohort',
    'cohorty',
    'cohorty',
    'skupiny',
    'skupina',
    'kurzy',
    'kurz',
    'třídy',
    'třída',
    'tridy',
    'trida',
  ],
  nationality: [
    'nationality',
    'citizenship',
    'národnost',
    'narodnost',
    'státní příslušnost',
    'státní_příslušnost',
    'statni prislusnost',
    'statni_prislusnost',
  ],
  birth_date: [
    'birth_date',
    'date_of_birth',
    'dob',
    'datum narození',
    'datum_narození',
    'datum narozeni',
    'datum_narozeni',
  ],
  prefix_title: [
    'prefix_title',
    'title_before',
    'titul před',
    'titul_před',
    'titul pred',
    'titul_pred',
  ],
  suffix_title: [
    'suffix_title',
    'title_after',
    'titul za',
    'titul_za',
    'titul za jménem',
    'titul_za_jménem',
    'titul za jmenem',
    'titul_za_jmenem',
  ],
  bio: ['bio', 'notes', 'description', 'poznámky', 'poznamky'],
  tax_identification_number: [
    'tax_identification_number',
    'tax_id',
    'tin',
    'daňové identifikační číslo',
    'daňové_identifikační_číslo',
    'danove identifikacni cislo',
    'danove_identifikacni_cislo',
    'dič',
    'dic',
  ],
  national_id_number: [
    'national_id_number',
    'id_number',
    'personal_number',
    'rodné číslo',
    'rodné_číslo',
    'rodne cislo',
    'rodne_cislo',
  ],
  csts_id: ['csts_id', 'csts'],
  wdsf_id: ['wdsf_id', 'wdsf'],
  external_ids: [
    'external_ids',
    'external_id',
    'external identifiers',
    'external_identifiers',
    'external-ids',
    'externí id',
    'externí_id',
    'externi id',
    'externi_id',
  ],
};

const EXTRA_TEXT_FIELDS = [
  'email',
  'phone',
  'prefix_title',
  'suffix_title',
  'bio',
  'tax_identification_number',
  'national_id_number',
  'csts_id',
  'wdsf_id',
] as const satisfies readonly PersonField[];

const OPTIONAL_FIELDS = [
  'gender',
  'nationality',
  'birth_date',
  ...EXTRA_TEXT_FIELDS,
  'external_ids',
] as const satisfies readonly PersonField[];

type ParserContext = { rowNumber: number };

type FieldParser = (value: string | undefined, context: ParserContext) => unknown;

function sanitiseHeaderKey(input: string): string {
  return input
    .trim()
    .toLowerCase()
    .replace(/[\s-]+/g, '_')
    .replace(/[^\p{Letter}\p{Number}_]+/gu, '')
    .replace(/_+/g, '_')
    .replace(/^_|_$/g, '');
}

const HEADER_ALIAS_MAP = new Map<string, FieldAliasKey>(
  Object.entries(RAW_PERSON_FIELD_ALIASES).flatMap(([field, aliases]) =>
    aliases.map((alias) => [sanitiseHeaderKey(alias), field as FieldAliasKey]),
  ),
);

function normaliseHeaderKey(input: string): string {
  const normalised = sanitiseHeaderKey(input);
  return HEADER_ALIAS_MAP.get(normalised) ?? normalised;
}

function printUsage() {
  const script = path.basename(process.argv[1] ?? 'import_people_from_csv.ts');
  console.error(
    `Usage: yarn workspace rozpisovnik-api import:people <file> [--tenant-id <id>] [--default-nationality <value>] [--create-missing-cohorts] [--default-cohort-color <hex>] [--sync-memberships] [--dry-run]\n`,
  );
  console.error('Environment: set DATABASE_URL for the target PostgreSQL instance.');
  console.error(`You are running ${script}.`);
}

function parseArgs(argv: readonly string[]): ImportArgs {
  const args: ImportArgs = {
    createMissingCohorts: false,
    syncMemberships: false,
    dryRun: false,
  };

  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    switch (arg) {
      case '--tenant-id': {
        const value = argv[++index];
        if (!value) {
          throw new Error('Missing value for --tenant-id');
        }
        const tenantId = Number(value);
        if (!Number.isFinite(tenantId) || tenantId <= 0) {
          throw new Error('Invalid tenant id. Provide a positive integer.');
        }
        args.tenantId = tenantId;
        break;
      }
      case '--default-nationality': {
        const value = argv[++index];
        if (!value) {
          throw new Error('Missing value for --default-nationality');
        }
        args.defaultNationality = value;
        break;
      }
      case '--default-cohort-color': {
        const value = argv[++index];
        if (!value) {
          throw new Error('Missing value for --default-cohort-color');
        }
        args.defaultCohortColor = value;
        break;
      }
      case '--create-missing-cohorts':
        args.createMissingCohorts = true;
        break;
      case '--sync-memberships':
        args.syncMemberships = true;
        break;
      case '--dry-run':
        args.dryRun = true;
        break;
      case '--help':
      case '-h':
        args.help = true;
        break;
      default:
        if (arg.startsWith('--')) {
          throw new Error(`Unknown option: ${arg}`);
        }
        if (args.file) {
          throw new Error('Multiple file paths provided. Only one CSV file can be imported at a time.');
        }
        args.file = arg;
        break;
    }
  }

  return args;
}

const FIELD_PARSERS: Partial<Record<PersonField, FieldParser>> = {
  gender: (value) => normaliseGender(value) ?? undefined,
  birth_date: (value, { rowNumber }) => {
    if (!value) {
      return undefined;
    }
    const parsed = normaliseBirthDate(value);
    if (!parsed) {
      console.warn(`Row ${rowNumber}: could not parse birth date "${value}", ignoring value.`);
    }
    return parsed ?? undefined;
  },
  external_ids: (value) => {
    const ids = collectList(value, { sort: true });
    return ids.length ? ids : undefined;
  },
};

function normaliseGender(value: string | undefined): string | null {
  if (!value) {
    return null;
  }

  const lookup: Record<string, string> = {
    man: 'man',
    male: 'man',
    m: 'man',
    boy: 'man',
    muž: 'man',
    muz: 'man',
    gentleman: 'man',
    woman: 'woman',
    female: 'woman',
    f: 'woman',
    girl: 'woman',
    žena: 'woman',
    zena: 'woman',
    unspecified: 'unspecified',
    other: 'unspecified',
    unknown: 'unspecified',
    ostatní: 'unspecified',
    ostatni: 'unspecified',
  };

  const key = value.trim().toLowerCase();
  return lookup[key] ?? 'unspecified';
}

function collectList(value: string | undefined, options: { sort?: boolean } = {}): string[] {
  if (!value) {
    return [];
  }

  const entries = value
    .split(',')
    .map((entry) => entry.trim())
    .filter(Boolean);
  const unique = [...new Set(entries)];
  if (options.sort) {
    unique.sort((left, right) => left.localeCompare(right));
  }
  return unique;
}

function getField(record: CsvRecord, key: FieldAliasKey): string | undefined {
  if (!(key in record)) {
    return undefined;
  }

  const raw = record[key];
  if (raw === undefined || raw === null) {
    return undefined;
  }

  const value = String(raw).trim();
  return value ? value : undefined;
}

function normaliseBirthDate(value: string | undefined): string | null {
  if (!value) {
    return null;
  }
  const trimmed = value.trim();
  if (!trimmed) {
    return null;
  }

  if (/^\d{4}-\d{2}-\d{2}$/.test(trimmed)) {
    return trimmed;
  }

  const dotMatch = trimmed.match(/^(\d{1,2})[.\/-](\d{1,2})[.\/-](\d{4})$/);
  if (dotMatch) {
    const [, day, month, year] = dotMatch;
    const normalisedDay = day.padStart(2, '0');
    const normalisedMonth = month.padStart(2, '0');
    return `${year}-${normalisedMonth}-${normalisedDay}`;
  }

  return null;
}

function parseCsv(content: string): CsvRecord[] {
  return parse(content, {
    columns: (header: string[]) => header.map((column) => normaliseHeaderKey(String(column))),
    skip_empty_lines: true,
    relax_column_count: true,
    trim: true,
  }) as CsvRecord[];
}

async function setTenant(client: Queryable, tenantId?: number) {
  if (!tenantId) {
    return;
  }

  await client.query('select set_config($1, $2, false)', ['jwt.claims.tenant_id', String(tenantId)]);
}

function buildPersonFromRecord(record: CsvRecord, rowNumber: number): BuildResult | null {
  const firstName = getField(record, 'first_name');
  const lastName = getField(record, 'last_name');

  if (!firstName || !lastName) {
    return null;
  }

  const person: PersonInput = {
    first_name: firstName,
    last_name: lastName,
  };
  const providedFields = new Set<PersonField>(['first_name', 'last_name']);

  for (const field of OPTIONAL_FIELDS) {
    const raw = getField(record, field);
    const parser = FIELD_PARSERS[field];
    const parsed = parser ? parser(raw, { rowNumber }) : raw;
    if (parsed === undefined || parsed === null) {
      continue;
    }
    (person as Record<PersonField, unknown>)[field] = parsed;
    providedFields.add(field);
  }

  const cohortNames = collectList(getField(record, 'cohorts'));

  return { person, providedFields, cohortNames };
}

async function findExistingPerson(
  client: Queryable,
  person: Pick<PersonInput, 'email' | 'first_name' | 'last_name'>,
) {
  if (person.email) {
    const { rows } = await client.query('select * from person where email = $1::citext limit 1', [person.email]);
    if (rows[0]) {
      return rows[0] as pg.QueryResultRow;
    }
  }

  const { rows } = await client.query(
    'select * from person where lower(first_name) = lower($1) and lower(last_name) = lower($2) order by id limit 1',
    [person.first_name, person.last_name],
  );
  return (rows[0] as pg.QueryResultRow) ?? null;
}

async function createPerson(client: Queryable, person: PersonInput, options: ImportArgs) {
  const payload: Record<string, unknown> = { ...person };
  payload.gender ??= 'unspecified';
  payload.nationality ??= options.defaultNationality ?? DEFAULT_NATIONALITY;

  const entries = Object.entries(payload).filter(([, value]) => value !== undefined);
  const columns = entries.map(([key]) => key);
  const values = entries.map(([, value]) => value);
  const placeholders = columns.map((_, index) => `$${index + 1}`);

  const { rows } = await client.query(
    `insert into person (${columns.join(', ')}) values (${placeholders.join(', ')}) returning *`,
    values,
  );
  return rows[0] as pg.QueryResultRow;
}

async function updatePerson(
  client: Queryable,
  existingPerson: pg.QueryResultRow,
  updates: PersonInput,
  providedFields: Set<PersonField>,
) {
  const fields: string[] = [];
  const values: unknown[] = [];

  for (const column of providedFields) {
    const value = updates[column];
    if (value === undefined) {
      continue;
    }
    const current = existingPerson[column];
    if (valuesEqual(current, value)) {
      continue;
    }
    fields.push(`${column} = $${fields.length + 1}`);
    values.push(value);
  }

  if (!fields.length) {
    return { person: existingPerson, updated: false };
  }

  values.push(existingPerson.id);
  const { rows } = await client.query(
    `update person set ${fields.join(', ')}, updated_at = now() where id = $${fields.length + 1} returning *`,
    values,
  );
  return { person: rows[0] as pg.QueryResultRow, updated: true };
}

function valuesEqual(left: unknown, right: unknown): boolean {
  if (Array.isArray(left) || Array.isArray(right)) {
    const arrayLeft = Array.isArray(left) ? left : left == null ? [] : [left];
    const arrayRight = Array.isArray(right) ? right : right == null ? [] : [right];

    if (arrayLeft.length !== arrayRight.length) {
      return false;
    }

    const sortedLeft = [...arrayLeft].map(String).sort();
    const sortedRight = [...arrayRight].map(String).sort();

    return sortedLeft.every((value, index) => value === sortedRight[index]);
  }

  return left === right;
}

async function resolveCohort(
  client: Queryable,
  cache: Map<string, number>,
  name: string,
  options: ImportArgs,
): Promise<number> {
  const cacheKey = name.toLowerCase();
  if (cache.has(cacheKey)) {
    return cache.get(cacheKey)!;
  }

  const existing = await client.query(
    'select id from cohort where tenant_id = current_tenant_id() and lower(name) = lower($1) limit 1',
    [name],
  );
  if (existing.rows[0]) {
    const cohortId = Number(existing.rows[0].id);
    cache.set(cacheKey, cohortId);
    return cohortId;
  }

  if (!options.createMissingCohorts) {
    throw new Error(`Cohort "${name}" not found for tenant ${options.tenantId ?? 1}`);
  }

  const color = options.defaultCohortColor ?? DEFAULT_COHORT_COLOR;
  const { rows } = await client.query(
    `insert into cohort (name, color_rgb, description, location, is_visible, ordering)
     values ($1, $2, '', '', true, coalesce((select max(ordering) + 1 from cohort where tenant_id = current_tenant_id()), 1))
     returning id`,
    [name, color],
  );
  const cohortId = Number(rows[0].id);
  cache.set(cacheKey, cohortId);
  return cohortId;
}

async function ensureCohortMembership(client: Queryable, personId: number, cohortId: number) {
  const existing = await client.query(
    'select id, status from cohort_membership where person_id = $1 and cohort_id = $2 order by since desc limit 1',
    [personId, cohortId],
  );

  if (existing.rows[0]) {
    const membership = existing.rows[0] as pg.QueryResultRow;
    if (membership.status === 'active') {
      return { created: false, updated: false };
    }
    await client.query(
      `update cohort_membership
         set status = 'active', until = null, updated_at = now()
       where id = $1`,
      [membership.id],
    );
    return { created: false, updated: true };
  }

  await client.query(
    `insert into cohort_membership (person_id, cohort_id, since, status)
     values ($1, $2, now(), 'active')`,
    [personId, cohortId],
  );
  return { created: true, updated: false };
}

async function fetchLatestMembershipStatuses(client: Queryable, personId: number) {
  const { rows } = await client.query(
    `select distinct on (cohort_id) cohort_id, status
       from cohort_membership
      where person_id = $1
      order by cohort_id, since desc`,
    [personId],
  );

  return rows.map((row) => ({ cohortId: Number(row.cohort_id), status: row.status as string }));
}

async function syncCohortMemberships(client: Queryable, personId: number, cohortIds: readonly number[]) {
  const before = await fetchLatestMembershipStatuses(client, personId);

  await client.query('select sync_cohort_memberships($1, $2::bigint[])', [personId, cohortIds]);

  const after = await fetchLatestMembershipStatuses(client, personId);

  const beforeByCohort = new Map(before.map((entry) => [entry.cohortId, entry.status]));

  let created = 0;
  let reactivated = 0;

  for (const entry of after) {
    if (entry.status !== 'active') {
      continue;
    }
    if (!beforeByCohort.has(entry.cohortId)) {
      created += 1;
      continue;
    }
    if (beforeByCohort.get(entry.cohortId) !== 'active') {
      reactivated += 1;
    }
  }

  const afterActive = new Set(after.filter((entry) => entry.status === 'active').map((entry) => entry.cohortId));
  let deactivated = 0;
  for (const entry of before) {
    if (entry.status === 'active' && !afterActive.has(entry.cohortId)) {
      deactivated += 1;
    }
  }

  return { created, reactivated, deactivated };
}

async function main() {
  const argv = process.argv.slice(2);
  let args: ImportArgs;
  try {
    args = parseArgs(argv);
  } catch (error) {
    console.error((error as Error).message);
    printUsage();
    process.exitCode = 1;
    return;
  }

  if (args?.help) {
    printUsage();
    return;
  }

  if (!args.file) {
    printUsage();
    process.exitCode = 1;
    return;
  }

  const connectionString = process.env.DATABASE_URL;
  if (!connectionString) {
    console.error('DATABASE_URL must be set to import data.');
    process.exitCode = 1;
    return;
  }

  const csvPath = path.resolve(args.file);
  let fileContent: string;
  try {
    fileContent = await readFile(csvPath, 'utf8');
  } catch (error) {
    console.error(`Unable to read ${csvPath}:`, (error as Error).message);
    process.exitCode = 1;
    return;
  }

  const records = parseCsv(fileContent);
  if (!Array.isArray(records) || !records.length) {
    console.error('The CSV file does not contain any records.');
    return;
  }

  const client = new Client({ connectionString });
  await client.connect();

  try {
    await setTenant(client, args.tenantId);

    const stats: Stats = {
      processed: 0,
      createdPeople: 0,
      updatedPeople: 0,
      createdMemberships: 0,
      reactivatedMemberships: 0,
      deactivatedMemberships: 0,
      syncedMembershipLists: 0,
      skippedRows: 0,
    };
    const cohortCache = new Map<string, number>();

    for (const [index, row] of records.entries()) {
      const rowNumber = index + 2; // account for header line
      const personResult = buildPersonFromRecord(row, rowNumber);

      if (!personResult) {
        console.warn(`Row ${rowNumber}: missing first or last name, skipping.`);
        stats.skippedRows += 1;
        continue;
      }

      const { person, providedFields, cohortNames } = personResult;

      if (!cohortNames.length) {
        console.warn(`Row ${rowNumber}: no cohorts provided, skipping.`);
        stats.skippedRows += 1;
        continue;
      }

      stats.processed += 1;
      await client.query('BEGIN');
      try {
        let personRecord = await findExistingPerson(client, person);
        let createdPerson = false;
        let updatedPerson = false;
        if (personRecord) {
          const result = await updatePerson(client, personRecord, person, providedFields);
          personRecord = result.person;
          updatedPerson = result.updated;
          if (updatedPerson) {
            stats.updatedPeople += 1;
          }
        } else {
          personRecord = await createPerson(client, person, args);
          createdPerson = true;
          stats.createdPeople += 1;
        }

        const cohortIds: number[] = [];
        for (const cohortName of cohortNames) {
          const cohortId = await resolveCohort(client, cohortCache, cohortName, args);
          cohortIds.push(cohortId);
          if (!args.syncMemberships) {
            const result = await ensureCohortMembership(client, Number(personRecord.id), cohortId);
            if (result.created) {
              stats.createdMemberships += 1;
            } else if (result.updated) {
              stats.reactivatedMemberships += 1;
            }
          }
        }

        if (args.syncMemberships) {
          const { created, reactivated, deactivated } = await syncCohortMemberships(
            client,
            Number(personRecord.id),
            cohortIds,
          );
          stats.createdMemberships += created;
          stats.reactivatedMemberships += reactivated;
          stats.deactivatedMemberships += deactivated;
          stats.syncedMembershipLists += 1;
        }

        if (args.dryRun) {
          await client.query('ROLLBACK');
        } else {
          await client.query('COMMIT');
        }

        const displayName = `${personRecord.first_name} ${personRecord.last_name}`.trim();
        const cohortSummary = cohortNames.join(', ');
        if (createdPerson) {
          console.log(`Row ${rowNumber}: created person ${displayName} (cohorts: ${cohortSummary}).`);
        } else if (updatedPerson) {
          console.log(`Row ${rowNumber}: updated person ${displayName} (cohorts: ${cohortSummary}).`);
        } else {
          console.log(`Row ${rowNumber}: no changes for person ${displayName} (cohorts: ${cohortSummary}).`);
        }
      } catch (error) {
        await client.query('ROLLBACK');
        throw new Error(`Row ${rowNumber}: ${(error as Error).message}`);
      }
    }

    console.info('\nImport summary');
    console.info('---------------');
    console.info(`Processed rows: ${stats.processed}`);
    console.info(`People created: ${stats.createdPeople}`);
    console.info(`People updated: ${stats.updatedPeople}`);
    console.info(`Memberships created: ${stats.createdMemberships}`);
    console.info(`Memberships reactivated: ${stats.reactivatedMemberships}`);
    if (args.syncMemberships) {
      console.info(`Memberships deactivated: ${stats.deactivatedMemberships}`);
      console.info(`Membership sets synced: ${stats.syncedMembershipLists}`);
    }
    console.info(`Rows skipped: ${stats.skippedRows}`);
  } finally {
    await client.end();
  }
}

void main().catch((error) => {
  console.error(error instanceof Error ? error.message : error);
  process.exitCode = 1;
});
