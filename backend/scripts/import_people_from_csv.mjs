#!/usr/bin/env node

// Usage: yarn workspace rozpisovnik-api import:people ./path/to/file.csv [--tenant-id=1] [--dry-run]
// Requires the DATABASE_URL environment variable to point to the target PostgreSQL instance.

import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import process from 'node:process';
import pg from 'pg';

const { Client } = pg;

function parseArgs(argv) {
  const args = {
    filePath: null,
    tenantId: 1,
    dryRun: false,
    defaultNationality: 'Unknown',
    since: null,
  };

  for (const arg of argv) {
    if (arg === '--dry-run') {
      args.dryRun = true;
      continue;
    }
    if (arg.startsWith('--tenant-id=')) {
      const value = Number.parseInt(arg.slice('--tenant-id='.length), 10);
      if (Number.isNaN(value) || value <= 0) {
        throw new Error(`Invalid tenant id provided: ${arg}`);
      }
      args.tenantId = value;
      continue;
    }
    if (arg.startsWith('--default-nationality=')) {
      const value = arg.slice('--default-nationality='.length).trim();
      if (!value) {
        throw new Error('Default nationality cannot be empty.');
      }
      args.defaultNationality = value;
      continue;
    }
    if (arg.startsWith('--since=')) {
      const raw = arg.slice('--since='.length).trim();
      if (!raw) {
        throw new Error('Provided since timestamp cannot be empty.');
      }
      const parsed = new Date(raw);
      if (Number.isNaN(parsed.getTime())) {
        throw new Error(`Cannot parse --since value \"${raw}\" as a valid date.`);
      }
      args.since = parsed.toISOString();
      continue;
    }
    if (arg.startsWith('-')) {
      throw new Error(`Unknown option ${arg}`);
    }
    if (args.filePath) {
      throw new Error(`Unexpected argument ${arg}; a single CSV file path is expected.`);
    }
    args.filePath = arg;
  }

  if (!args.filePath) {
    throw new Error('Missing CSV file path argument.');
  }

  return args;
}

function parseCsv(content) {
  const rows = [];
  let currentRow = [];
  let currentField = '';
  let insideQuotes = false;

  for (let i = 0; i < content.length; i += 1) {
    const char = content[i];
    if (insideQuotes) {
      if (char === '"') {
        const next = content[i + 1];
        if (next === '"') {
          currentField += '"';
          i += 1;
        } else {
          insideQuotes = false;
        }
      } else {
        currentField += char;
      }
      continue;
    }

    if (char === '"') {
      insideQuotes = true;
      continue;
    }

    if (char === ',') {
      currentRow.push(currentField);
      currentField = '';
      continue;
    }

    if (char === '\r') {
      continue;
    }

    if (char === '\n') {
      currentRow.push(currentField);
      currentField = '';
      if (currentRow.length > 0) {
        rows.push(currentRow);
      }
      currentRow = [];
      continue;
    }

    currentField += char;
  }

  if (insideQuotes) {
    throw new Error('Unterminated quoted field in CSV input.');
  }

  if (currentField.length > 0 || currentRow.length > 0) {
    currentRow.push(currentField);
    rows.push(currentRow);
  }

  return rows.filter((row) => row.some((value) => value.trim() !== ''));
}

function normaliseHeader(value) {
  return value.trim().toLowerCase();
}

const GENDER_MAP = new Map([
  ['man', 'man'],
  ['male', 'man'],
  ['m', 'man'],
  ['woman', 'woman'],
  ['female', 'woman'],
  ['f', 'woman'],
  ['unspecified', 'unspecified'],
  ['unknown', 'unspecified'],
  ['other', 'unspecified'],
  ['n/a', 'unspecified'],
]);

function extractField(record, keys, { required = false, label }) {
  for (const key of keys) {
    if (key in record && record[key] != null) {
      return record[key];
    }
  }
  if (required) {
    throw new Error(`Missing required column for ${label ?? keys[0]}.`);
  }
  return '';
}

function mapGender(raw) {
  const normalised = raw.trim().toLowerCase();

  const mapped = GENDER_MAP.get(normalised);
  if (!mapped) {
    throw new Error(`Unsupported gender value \"${raw}\".`);
  }
  return mapped;
}

async function findExistingPerson(client, { email, phone, firstName, lastName }) {
  if (email) {
    const { rows } = await client.query(
      'select id from person where lower(email) = lower($1) limit 1',
      [email],
    );
    if (rows[0]) {
      return { id: rows[0].id, reason: 'email' };
    }
  }

  if (phone) {
    const { rows } = await client.query(
      'select id from person where phone = $1 limit 1',
      [phone],
    );
    if (rows[0]) {
      return { id: rows[0].id, reason: 'phone' };
    }
  }

  const { rows } = await client.query(
    'select id from person where lower(first_name) = lower($1) and lower(last_name) = lower($2) limit 1',
    [firstName, lastName],
  );
  if (rows[0]) {
    return { id: rows[0].id, reason: 'name' };
  }

  return null;
}

async function ensureCohort(client, cache, tenantId, cohortName) {
  const key = cohortName.toLowerCase();
  if (cache.has(key)) {
    return cache.get(key);
  }

  const { rows } = await client.query(
    'select id from cohort where tenant_id = $1 and lower(name) = lower($2) limit 1',
    [tenantId, cohortName],
  );

  if (!rows[0]) {
    throw new Error(`Cannot find cohort named \"${cohortName}\" for tenant ${tenantId}.`);
  }

  const cohort = rows[0];
  cache.set(key, cohort);
  return cohort;
}

async function personHasActiveMembership(client, personId, cohortId, tenantId) {
  const { rows } = await client.query(
    `select id
       from cohort_membership
      where cohort_id = $1
        and person_id = $2
        and tenant_id = $3
        and status = 'active'
      limit 1`,
    [cohortId, personId, tenantId],
  );
  return Boolean(rows[0]);
}

async function main() {
  const args = parseArgs(process.argv.slice(2));
  const databaseUrl = process.env.DATABASE_URL;
  if (!databaseUrl) {
    throw new Error('DATABASE_URL environment variable must be set.');
  }

  const csvPath = resolve(process.cwd(), args.filePath);
  const csvContent = await readFile(csvPath, 'utf8');
  const rows = parseCsv(csvContent);
  if (rows.length === 0) {
    throw new Error('The provided CSV file does not contain any data.');
  }

  const headerRow = rows[0].map(normaliseHeader);
  const dataRows = rows.slice(1);

  const records = dataRows.map((row, index) => {
    const record = {};
    headerRow.forEach((header, columnIndex) => {
      record[header] = (row[columnIndex] ?? '').trim();
    });
    return { record, line: index + 2 };
  });

  const client = new Client({ connectionString: databaseUrl });
  await client.connect();

  const stats = {
    createdPeople: 0,
    reusedPeople: 0,
    createdMemberships: 0,
    skippedMemberships: 0,
  };

  const cohortCache = new Map();

  try {
    for (const { record, line } of records) {
      const firstName = extractField(record, ['name', 'first_name', 'firstname'], { required: true, label: 'first name' });
      const lastName = extractField(record, ['surname', 'last_name', 'lastname'], { required: true, label: 'surname' });
      const genderRaw = extractField(record, ['gender'], { required: true, label: 'gender' });
      const emailRaw = extractField(record, ['email', 'e-mail'], { label: 'email' });
      const phoneRaw = extractField(record, ['phone', 'phone_number'], { label: 'phone' });
      const cohortsRaw = extractField(record, ['cohorts', 'cohort_names', 'cohort'], { label: 'cohorts' });

      const gender = mapGender(genderRaw);
      const email = emailRaw ? emailRaw.toLowerCase() : null;
      const phone = phoneRaw || null;
      const cohortNames = cohortsRaw
        ? cohortsRaw
            .split(',')
            .map((item) => item.trim())
            .filter((item) => item.length > 0)
        : [];

      if (cohortNames.length === 0) {
        throw new Error(`Row ${line}: at least one cohort must be provided.`);
      }

      await client.query('BEGIN');
      try {
        const existing = await findExistingPerson(client, {
          email,
          phone,
          firstName,
          lastName,
        });

        let personId = existing?.id ?? null;
        if (existing) {
          stats.reusedPeople += 1;
        } else if (args.dryRun) {
          stats.createdPeople += 1;
        } else {
          const insertPerson = await client.query(
            `insert into person (first_name, last_name, gender, nationality, email, phone)
             values ($1, $2, $3, $4, $5, $6)
             returning id`,
            [firstName, lastName, gender, args.defaultNationality, email, phone],
          );
          personId = insertPerson.rows[0].id;
          stats.createdPeople += 1;
        }

        for (const cohortName of cohortNames) {
          const cohort = await ensureCohort(client, cohortCache, args.tenantId, cohortName);

          if (args.dryRun) {
            stats.createdMemberships += 1;
            continue;
          }

          if (!personId) {
            throw new Error('Unexpected missing person identifier when creating memberships.');
          }

          const alreadyMember = await personHasActiveMembership(client, personId, cohort.id, args.tenantId);
          if (alreadyMember) {
            stats.skippedMemberships += 1;
            continue;
          }

          const params = [cohort.id, personId, args.tenantId];
          let insertSql =
            'insert into cohort_membership (cohort_id, person_id, tenant_id) values ($1, $2, $3) returning id';
          if (args.since) {
            insertSql =
              'insert into cohort_membership (cohort_id, person_id, tenant_id, since) values ($1, $2, $3, $4) returning id';
            params.push(args.since);
          }
          await client.query(insertSql, params);
          stats.createdMemberships += 1;
        }

        await client.query('COMMIT');
      } catch (error) {
        await client.query('ROLLBACK');
        throw new Error(`Failed to import row ${line}: ${error.message}`);
      }
    }
  } finally {
    await client.end();
  }

  console.log('Import finished.');
  console.table(stats);
}

main().catch((error) => {
  console.error(error.message);
  process.exitCode = 1;
});
