import type pg from 'pg';
import { parse } from 'csv-parse/sync';

type Queryable = Pick<pg.Client, 'query'>;

export type ImportPeopleOptions = {
  defaultNationality?: string;
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
  csts_id?: number;
  wdsf_id?: number;
  external_ids?: string[];
};

type PersonField = keyof PersonInput;

export type ImportPeopleStats = {
  processed: number;
  createdPeople: number;
  updatedPeople: number;
  syncedMembershipLists: number;
  skippedRows: number;
};

export type ImportPeopleResult = {
  stats: ImportPeopleStats;
  diagnostics: { row: number; kind: 'skipped' | 'warning'; message: string }[];
};

type CsvRecord = Partial<Record<PersonField | 'cohorts', string>>;

const FIELD_ALIASES = {
  first_name: ['firstname', 'given_name', 'name', 'first', 'krestni jmeno', 'jmeno'],
  last_name: ['lastname', 'surname', 'family_name', 'familyname', 'prijmeni'],
  gender: ['sex', 'pohlavi'],
  email: ['email_address', 'e-mail'],
  phone: ['phone_number', 'telephone', 'telefon', 'telefonni cislo'],
  cohorts: ['cohort_names', 'cohort_name', 'cohort', 'cohorty', 'skupiny', 'skupina', 'kurzy', 'kurz', 'tridy', 'trida'],
  nationality: ['citizenship', 'narodnost', 'statni prislusnost'],
  birth_date: ['date_of_birth', 'dob', 'datum narozeni'],
  prefix_title: ['title_before', 'titul pred'],
  suffix_title: ['title_after', 'titul za', 'titul za jmenem'],
  bio: ['notes', 'description', 'poznamky'],
  tax_identification_number: ['tax_id', 'tin', 'personal_number', 'rodne cislo', 'danove identifikacni cislo', 'dic'],
  national_id_number: ['id_number', 'cislo op'],
  csts_id: ['csts'],
  wdsf_id: ['wdsf'],
  external_ids: ['external_id', 'external identifiers', 'externi id'],
} satisfies Record<keyof CsvRecord, string[]>;

function sanitiseHeaderKey(input: string): string {
  return input
    .normalize('NFD')
    .replaceAll(/\p{Mark}/gu, '')
    .trim()
    .toLowerCase()
    .replaceAll(/[\s-]+/g, '_')
    .replaceAll(/[^\p{Letter}\p{Number}_]+/gu, '')
    .replaceAll(/_+/g, '_')
    .replaceAll(/^_|_$/g, '');
}

const HEADER_ALIAS_MAP = new Map(
  Object.entries(FIELD_ALIASES).flatMap(([field, aliases]) =>
    [field, ...aliases].map((alias) => [sanitiseHeaderKey(alias), field]),
  ),
);

function normaliseFederationId(value: string | undefined): number | null {
  if (!value) {
    return null;
  }

  if (!/^\d+$/.test(value)) {
    return null;
  }

  const parsed = Number(value);
  return Number.isInteger(parsed) && parsed > 0 && parsed <= 2_147_483_647
    ? parsed
    : null;
}

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

  const dotMatch = trimmed.match(/^(\d{1,2})[./-](\d{1,2})[./-](\d{4})$/);
  if (dotMatch) {
    const [, day = '', month = '', year] = dotMatch;
    const normalisedDay = day.padStart(2, '0');
    const normalisedMonth = month.padStart(2, '0');
    return `${year}-${normalisedMonth}-${normalisedDay}`;
  }

  return null;
}

function buildPersonFromRecord(record: CsvRecord, row: number, diagnostics: ImportPeopleResult['diagnostics']): PersonInput | null {
  if (!record.first_name || !record.last_name) {
    diagnostics.push({ row, kind: 'skipped', message: 'Missing first or last name.' });
    return null;
  }
  const birthDate = normaliseBirthDate(record.birth_date);
  if (record.birth_date && !birthDate) {
    diagnostics.push({ row, kind: 'warning', message: `Could not parse birth date "${record.birth_date}"; value ignored.` });
  }
  const externalIds = collectList(record.external_ids, { sort: true });
  const cstsId = normaliseFederationId(record.csts_id);
  const wdsfId = normaliseFederationId(record.wdsf_id);
  for (const [field, parsed] of [['csts_id', cstsId], ['wdsf_id', wdsfId]] as const) {
    if (record[field] && parsed === null) {
      diagnostics.push({ row, kind: 'warning', message: `Invalid ${field} "${record[field]}"; expected a positive integer up to 2147483647. Value ignored.` });
    }
  }
  return {
    first_name: record.first_name,
    last_name: record.last_name,
    gender: normaliseGender(record.gender) ?? undefined,
    nationality: record.nationality,
    birth_date: birthDate ?? undefined,
    email: record.email,
    phone: record.phone,
    prefix_title: record.prefix_title,
    suffix_title: record.suffix_title,
    bio: record.bio,
    tax_identification_number: record.tax_identification_number,
    national_id_number: record.national_id_number,
    csts_id: cstsId ?? undefined,
    wdsf_id: wdsfId ?? undefined,
    external_ids: externalIds.length > 0 ? externalIds : undefined,
  };
}

async function findExistingPerson(
  client: Queryable,
  person: PersonInput,
) {
  if (person.external_ids?.length) {
    const { rows } = await client.query(
      'select * from person where external_ids && $1::text[] order by id limit 2',
      [person.external_ids],
    );
    if (rows.length > 1) {
      throw new Error(`External IDs match multiple people (IDs: ${rows.map((row) => row.id).join(', ')}). Resolve the conflict before importing.`);
    }
    if (rows[0]) return rows[0];
  }
  if (person.email) {
    const { rows } = await client.query('select * from person where email = $1::citext order by id limit 2', [person.email]);
    if (rows.length > 1) {
      throw new Error(`Multiple people match email "${person.email}" (IDs: ${rows.map((row) => row.id).join(', ')}). Resolve the duplicate before importing.`);
    }
    if (rows[0]) {
      return rows[0] as pg.QueryResultRow;
    }
  }

  const { rows } = await client.query(
    'select * from person where lower(first_name) = lower($1) and lower(last_name) = lower($2) order by id limit 2',
    [person.first_name, person.last_name],
  );
  if (rows.length > 1) {
    throw new Error(`Multiple people match name "${person.first_name} ${person.last_name}" (IDs: ${rows.map((row) => row.id).join(', ')}). Supply a unique email or resolve the duplicate before importing.`);
  }
  return rows[0] ?? null;
}

async function createPerson(client: Queryable, person: PersonInput, options: ImportPeopleOptions) {
  const { rows } = await client.query(
    `select * from create_person(null, jsonb_populate_record(null::person, $1::jsonb), true, false, false, false, now())`,
    [{
      ...person,
      gender: person.gender ?? 'unspecified',
      nationality: person.nationality ?? options.defaultNationality ?? '203',
      prefix_title: person.prefix_title ?? '',
      suffix_title: person.suffix_title ?? '',
      bio: person.bio ?? '',
    }],
  );
  return rows[0];
}

async function updatePerson(
  client: Queryable,
  existingPerson: pg.QueryResultRow,
  updates: PersonInput,
) {
  const fields: string[] = [];
  const values: unknown[] = [];

  for (const [column, value] of Object.entries(updates)) {
    if (value === undefined) {
      continue;
    }
    const current = existingPerson[column];
    const unchanged = column === 'external_ids'
      ? JSON.stringify((current ?? []).toSorted()) === JSON.stringify((updates.external_ids ?? []).toSorted())
      : current === value;
    if (unchanged) {
      continue;
    }
    fields.push(`${column} = $${fields.length + 1}`);
    values.push(value);
  }

  if (fields.length === 0) {
    return { person: existingPerson, updated: false };
  }

  values.push(existingPerson.id);
  const { rows } = await client.query(
    `update person set ${fields.join(', ')}, updated_at = now() where id = $${fields.length + 1} returning *`,
    values,
  );
  return { person: rows[0] as pg.QueryResultRow, updated: true };
}

async function resolveCohort(
  client: Queryable,
  cache: Map<string, number>,
  name: string,
): Promise<number> {
  const cacheKey = name.toLowerCase();
  if (cache.has(cacheKey)) {
    return cache.get(cacheKey)!;
  }

  const existing = await client.query(
    'select id from cohort where tenant_id = current_tenant_id() and lower(name) = lower($1) limit 2',
    [name],
  );
  if (existing.rows.length !== 1) {
    throw new Error(`Cohort "${name}" ${existing.rows.length > 0 ? 'is ambiguous' : 'does not exist; create it before importing'}.`);
  }
  const cohortId = Number(existing.rows[0].id);
  cache.set(cacheKey, cohortId);
  return cohortId;
}

/**
 * Run with a connected client inside a caller-owned transaction.
 * Nonempty cohorts replace active memberships; absent or empty cohorts leave them unchanged.
 * Commit after success, or roll back for a dry run or any error.
 * The caller configures tenant and role settings; this function never commits or closes the client.
 */
export async function importPeopleFromCsv(
  client: Queryable,
  content: string,
  options: ImportPeopleOptions = {},
): Promise<ImportPeopleResult> {
  const records: { record: CsvRecord; info: { lines: number } }[] = parse(content, {
    columns: (headers: string[]) => headers.map((header) => {
      const key = sanitiseHeaderKey(header);
      return HEADER_ALIAS_MAP.get(key) ?? key;
    }),
    cast: (value: string) => value.trim() || undefined,
    skip_empty_lines: true,
    relax_column_count: true,
    bom: true,
    info: true,
  });

  const stats: ImportPeopleStats = {
    processed: 0,
    createdPeople: 0,
    updatedPeople: 0,
    syncedMembershipLists: 0,
    skippedRows: 0,
  };
  const diagnostics: ImportPeopleResult['diagnostics'] = [];
  const cohortCache = new Map<string, number>();

  for (const { record, info } of records) {
    const row = info.lines;
    const person = buildPersonFromRecord(record, row, diagnostics);
    const cohortNames = collectList(record.cohorts);
    if (!person) {
      stats.skippedRows += 1;
      continue;
    }

    try {
      let personRecord = await findExistingPerson(client, person);
      if (personRecord) {
        const result = await updatePerson(client, personRecord, person);
        personRecord = result.person;
        if (result.updated) stats.updatedPeople += 1;
        await client.query(
          `insert into tenant_membership (person_id)
           select $1 where not exists (
             select 1 from tenant_membership
             where person_id = $1 and tenant_id = current_tenant_id() and status = 'active'
           )`,
          [personRecord.id],
        );
      } else {
        personRecord = await createPerson(client, person, options);
        stats.createdPeople += 1;
      }

      const cohortIds: number[] = [];
      for (const name of cohortNames) {
        const cohortId = await resolveCohort(client, cohortCache, name);
        cohortIds.push(cohortId);
      }
      if (cohortIds.length > 0) {
        await client.query('select sync_cohort_memberships($1, $2::bigint[])', [personRecord.id, [...new Set(cohortIds)]]);
        stats.syncedMembershipLists += 1;
      }
      stats.processed += 1;
    } catch (error) {
      throw new Error(`Row ${row}: ${error instanceof Error ? error.message : String(error)}`, { cause: error });
    }
  }
  return { stats, diagnostics };
}
