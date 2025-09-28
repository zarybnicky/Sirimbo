#!/usr/bin/env node
import { readFile } from 'node:fs/promises';
import path from 'node:path';
import process from 'node:process';
import pg from 'pg';
import { parse } from 'csv-parse/sync';

const { Client } = pg;

const DEFAULT_NATIONALITY = 'Czech Republic';
const DEFAULT_COHORT_COLOR = '#808080';
const PERSON_FIELD_ALIASES = {
  first_name: ['first_name', 'firstname', 'name'],
  last_name: ['last_name', 'lastname', 'surname'],
  gender: ['gender', 'sex'],
  email: ['email', 'e-mail', 'email_address'],
  phone: ['phone', 'phone_number', 'telephone'],
  cohorts: ['cohorts', 'cohort_names', 'cohort names'],
  nationality: ['nationality', 'citizenship'],
  birth_date: ['birth_date', 'date_of_birth', 'dob'],
  prefix_title: ['prefix_title', 'title_before'],
  suffix_title: ['suffix_title', 'title_after'],
  bio: ['bio', 'notes', 'description'],
  tax_identification_number: ['tax_identification_number', 'tax_id', 'tin'],
  national_id_number: ['national_id_number', 'id_number', 'personal_number'],
  csts_id: ['csts_id', 'csts'],
  wdsf_id: ['wdsf_id', 'wdsf'],
  external_ids: ['external_ids', 'external id', 'external-ids', 'external identifiers'],
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
];

function printUsage() {
  const script = path.basename(process.argv[1] ?? 'import_people_from_csv.mjs');
  console.error(
    `Usage: yarn workspace rozpisovnik-api import:people-from-csv <file> [--tenant-id <id>] [--default-nationality <value>] [--create-missing-cohorts] [--default-cohort-color <hex>] [--sync-memberships] [--dry-run]\n`,
  );
  console.error('Alias:  yarn workspace rozpisovnik-api import:cohort-members <file> [...options]\n');
  console.error(`(Directly with Node.js: node -r ./.pnp.cjs backend/scripts/${script} <file> [...options])`);
  console.error('Environment: set DATABASE_URL for the target PostgreSQL instance.');
}

function parseArgs(argv) {
  const args = {
    createMissingCohorts: false,
    dryRun: false,
    syncMemberships: false,
  };

  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    switch (arg) {
      case '--tenant-id': {
        const value = argv[++i];
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
        const value = argv[++i];
        if (!value) {
          throw new Error('Missing value for --default-nationality');
        }
        args.defaultNationality = value;
        break;
      }
      case '--default-cohort-color': {
        const value = argv[++i];
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
    }
  }

  return args;
}

function normaliseGender(value) {
  if (!value) {
    return null;
  }

  const lookup = {
    man: 'man',
    male: 'man',
    m: 'man',
    boy: 'man',
    woman: 'woman',
    female: 'woman',
    f: 'woman',
    girl: 'woman',
    unspecified: 'unspecified',
    other: 'unspecified',
    unknown: 'unspecified',
  };

  const key = value.toString().trim().toLowerCase();
  return lookup[key] ?? 'unspecified';
}

function parseCohortList(value) {
  if (!value) {
    return [];
  }
  const unique = new Set();
  for (const entry of value.split(',')) {
    const trimmed = entry.trim();
    if (trimmed) {
      unique.add(trimmed);
    }
  }
  return [...unique];
}

function getField(record, aliases) {
  for (const key of aliases) {
    if (!Object.hasOwn(record, key)) {
      continue;
    }
    const raw = record[key];
    if (raw === undefined || raw === null) {
      continue;
    }
    const value = String(raw).trim();
    if (value.length === 0) {
      continue;
    }
    return value;
  }
  return undefined;
}

function parseExternalIds(value) {
  if (!value) {
    return [];
  }
  const unique = new Set();
  for (const part of value.split(',')) {
    const trimmed = part.trim();
    if (trimmed.length === 0) {
      continue;
    }
    unique.add(trimmed);
  }
  return [...unique].sort((a, b) => a.localeCompare(b));
}

function normaliseBirthDate(value) {
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

function parseCsv(content) {
  return parse(content, {
    columns: (header) => header.map((column) => column.trim().toLowerCase()),
    skip_empty_lines: true,
    relax_column_count: true,
    trim: true,
  });
}

async function setTenant(client, tenantId) {
  if (!tenantId) {
    return;
  }
  await client.query('select set_config($1, $2, false)', ['jwt.claims.tenant_id', String(tenantId)]);
}

function buildPersonFromRecord(record, rowNumber) {
  const firstName = getField(record, PERSON_FIELD_ALIASES.first_name);
  const lastName = getField(record, PERSON_FIELD_ALIASES.last_name);

  if (!firstName || !lastName) {
    return null;
  }

  const person = {
    first_name: firstName,
    last_name: lastName,
  };
  const providedFields = new Set(['first_name', 'last_name']);

  const gender = normaliseGender(getField(record, PERSON_FIELD_ALIASES.gender));
  if (gender) {
    person.gender = gender;
    providedFields.add('gender');
  }

  const nationality = getField(record, PERSON_FIELD_ALIASES.nationality);
  if (nationality) {
    person.nationality = nationality;
    providedFields.add('nationality');
  }

  const birthDateRaw = getField(record, PERSON_FIELD_ALIASES.birth_date);
  if (birthDateRaw) {
    const birthDate = normaliseBirthDate(birthDateRaw);
    if (birthDate) {
      person.birth_date = birthDate;
      providedFields.add('birth_date');
    } else {
      console.warn(`Row ${rowNumber}: could not parse birth date "${birthDateRaw}", ignoring value.`);
    }
  }

  for (const column of EXTRA_TEXT_FIELDS) {
    const aliases = PERSON_FIELD_ALIASES[column];
    if (!aliases) {
      continue;
    }
    const value = getField(record, aliases);
    if (value) {
      person[column] = value;
      providedFields.add(column);
    }
  }

  const cohortValue = getField(record, PERSON_FIELD_ALIASES.cohorts) ?? '';
  const cohortNames = parseCohortList(cohortValue);

  const externalIdsAliases = PERSON_FIELD_ALIASES.external_ids;
  if (externalIdsAliases) {
    const externalIdsValue = getField(record, externalIdsAliases);
    if (externalIdsValue) {
      const externalIds = parseExternalIds(externalIdsValue);
      if (externalIds.length) {
        person.external_ids = externalIds;
        providedFields.add('external_ids');
      }
    }
  }

  return { person, providedFields, cohortNames };
}

async function findExistingPerson(client, { email, first_name: firstName, last_name: lastName }) {
  if (email) {
    const { rows } = await client.query(
      'select * from person where email = $1::citext limit 1',
      [email],
    );
    if (rows[0]) {
      return rows[0];
    }
  }

  const { rows } = await client.query(
    'select * from person where lower(first_name) = lower($1) and lower(last_name) = lower($2) order by id limit 1',
    [firstName, lastName],
  );
  return rows[0] ?? null;
}

async function createPerson(client, person, options) {
  const payload = { ...person };
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
  return rows[0];
}

async function updatePerson(client, existingPerson, updates, providedFields) {
  const fields = [];
  const values = [];

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
  return { person: rows[0], updated: true };
}

function valuesEqual(left, right) {
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

async function resolveCohort(client, cache, name, options) {
  const cacheKey = name.toLowerCase();
  if (cache.has(cacheKey)) {
    return cache.get(cacheKey);
  }

  const existing = await client.query(
    'select id from cohort where tenant_id = current_tenant_id() and lower(name) = lower($1) limit 1',
    [name],
  );
  if (existing.rows[0]) {
    cache.set(cacheKey, existing.rows[0].id);
    return existing.rows[0].id;
  }

  if (!options.createMissingCohorts) {
    throw new Error(`Cohort \"${name}\" not found for tenant ${options.tenantId ?? 1}`);
  }

  const color = options.defaultCohortColor ?? DEFAULT_COHORT_COLOR;
  const { rows } = await client.query(
    `insert into cohort (name, color_rgb, description, location, is_visible, ordering)
     values ($1, $2, '', '', true, coalesce((select max(ordering) + 1 from cohort where tenant_id = current_tenant_id()), 1))
     returning id`,
    [name, color],
  );
  const cohortId = rows[0].id;
  cache.set(cacheKey, cohortId);
  return cohortId;
}

async function ensureCohortMembership(client, personId, cohortId) {
  const existing = await client.query(
    'select id, status from cohort_membership where person_id = $1 and cohort_id = $2 order by since desc limit 1',
    [personId, cohortId],
  );

  if (existing.rows[0]) {
    const membership = existing.rows[0];
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

async function fetchLatestMembershipStatuses(client, personId) {
  const { rows } = await client.query(
    `select distinct on (cohort_id) cohort_id, status
       from cohort_membership
      where person_id = $1
      order by cohort_id, since desc`,
    [personId],
  );

  return rows.map((row) => ({ cohortId: Number(row.cohort_id), status: row.status }));
}

async function syncCohortMemberships(client, personId, cohortIds) {
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

  const afterActive = new Set(
    after.filter((entry) => entry.status === 'active').map((entry) => entry.cohortId),
  );
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
  let args;
  try {
    args = parseArgs(argv);
  } catch (error) {
    console.error(error.message);
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
  let fileContent;
  try {
    fileContent = await readFile(csvPath, 'utf8');
  } catch (error) {
    console.error(`Unable to read ${csvPath}:`, error.message);
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

    const stats = {
      processed: 0,
      createdPeople: 0,
      updatedPeople: 0,
      createdMemberships: 0,
      reactivatedMemberships: 0,
      deactivatedMemberships: 0,
      syncedMembershipLists: 0,
      skippedRows: 0,
    };
    const cohortCache = new Map();

    for (let index = 0; index < records.length; index += 1) {
      const row = records[index];
      const rowNumber = index + 2; // account for header line
      const personResult = buildPersonFromRecord(row, rowNumber);

      if (!personResult) {
        console.warn(`Row ${rowNumber}: missing name or surname, skipping.`);
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

        const uniqueCohortNames = [...new Set(cohortNames)];
        const cohortIds = [];
        for (const cohortName of uniqueCohortNames) {
          const cohortId = await resolveCohort(client, cohortCache, cohortName, args);
          cohortIds.push(cohortId);
          if (!args.syncMemberships) {
            const result = await ensureCohortMembership(client, personRecord.id, cohortId);
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
            personRecord.id,
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
        const cohortSummary = uniqueCohortNames.join(', ');
        if (createdPerson) {
          console.log(
            `Row ${rowNumber}: created person ${displayName} (cohorts: ${cohortSummary}).`,
          );
        } else if (updatedPerson) {
          console.log(
            `Row ${rowNumber}: updated person ${displayName} (cohorts: ${cohortSummary}).`,
          );
        } else {
          console.log(
            `Row ${rowNumber}: no changes for person ${displayName} (cohorts: ${cohortSummary}).`,
          );
        }
      } catch (error) {
        await client.query('ROLLBACK');
        throw new Error(`Row ${rowNumber}: ${error.message}`);
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

main().catch((error) => {
  console.error(error.message);
  process.exitCode = 1;
});
