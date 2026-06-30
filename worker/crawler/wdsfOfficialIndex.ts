import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import {
  ensurePeople,
  type gender,
  mergeCompetitionOfficials,
  type official_role,
} from './federated.queries.ts';
import { makePgtypedCollection } from './pgtypedCollection.ts';
import type { PoolClient } from 'pg';

const PERSON_REL = 'http://services.worlddancesport.org/rel/official/person';
const COMPETITION_REL = 'http://services.worlddancesport.org/rel/official/competition';
const OFFICIAL_NAME =
  /^\s*(Adjudicator|Chairperson|Scrutiny|DJ|Head Judge|Invigilator)(?:\s*\(([^)]*)\))?\s+(.+)\s*$/;

export const schema = z.array(
  z.object({
    link: z.array(z.object({ href: z.string(), rel: z.string(), type: z.string() })),
    Id: z.number(),
    Name: z.string(),
    country: z.string().nullable(),
  }),
);

type Official = z.infer<typeof schema>[number];
type WdsfOfficialRole =
  | 'Adjudicator'
  | 'Chairperson'
  | 'Scrutiny'
  | 'DJ'
  | 'Head Judge'
  | 'Invigilator';

export type ParsedWdsfOfficial = {
  officialExternalId: string;
  competitionExternalId: string;
  personExternalId: string;
  personId: string;
  roleText: WdsfOfficialRole;
  role: official_role | null;
  judgeLabel: string | null;
  canonicalName: string;
  country: string | null;
};

export const wdsfOfficialIndex: JsonLoader<z.infer<typeof schema>> = {
  mode: 'json',
  schema,
  buildRequest: (key) => ({
    url: new URL(`https://services.worlddancesport.org/api/1/official?competitionId=${key}`),
    init: {
      headers: {
        Authorization: process.env.WDSF_AUTH ?? '',
        Accept: 'application/json',
      },
    },
  }),
  revalidatePeriod: '7d',
  mapResponseToStatus({ httpStatus }) {
    if (httpStatus === 404) return 'gone';
    if (httpStatus === 500) return 'error';
    return undefined;
  },
  async load(client, parsed) {
    await loadWdsfOfficialIndex(client, parsed);
  },
};

async function loadWdsfOfficialIndex(client: PoolClient, parsed: z.infer<typeof schema>) {
  if (parsed.length === 0) return;

  const officials = parsed.map(parseWdsfOfficial);
  const competitionExternalIds = new Set(officials.map((x) => x.competitionExternalId));
  if (competitionExternalIds.size !== 1) {
    throw new Error(
      `WDSF officialIndex response contains multiple competitions: ${[
        ...competitionExternalIds,
      ].join(', ')}`,
    );
  }
  const [competitionExternalId] = competitionExternalIds;

  // TODO(wdsf): Preserve parsed official country and adjudicator labels instead
  // of only storing person, official external ID, and role.
  const people = makePgtypedCollection<{
    federation: string;
    externalId: string;
    canonicalName: string;
    gender: gender;
  }>(
    ['federation', 'externalId', 'canonicalName', 'gender'],
    ['federation', 'externalId'],
  );
  for (const official of officials) {
    people.add({
      federation: 'wdsf',
      externalId: official.personExternalId,
      canonicalName: official.canonicalName,
      gender: 'unknown',
    });
  }
  if (people.length) await ensurePeople.run(people.params, client);

  const competitionOfficials = makePgtypedCollection<{
    competitionExternalId: string;
    externalId: string;
    personId: string;
    role: official_role;
  }>(
    ['competitionExternalId', 'externalId', 'personId', 'role'],
    ['competitionExternalId', 'personId', 'role'],
  );
  for (const official of officials) {
    if (!official.role) continue;
    competitionOfficials.add({
      competitionExternalId,
      externalId: official.officialExternalId,
      personId: official.personId,
      role: official.role,
    });
  }
  await mergeCompetitionOfficials.run(
    {
      federation: 'wdsf',
      scopeCompetitionExternalId: [competitionExternalId],
      ...competitionOfficials.params,
    },
    client,
  );
}

export function parseWdsfOfficial(official: Official): ParsedWdsfOfficial {
  const match = OFFICIAL_NAME.exec(official.Name);
  if (!match) {
    throw new Error(`Unexpected WDSF official name format: ${official.Name}`);
  }

  const roleText = match[1] as WdsfOfficialRole;
  const label = match[2]?.trim() || null;
  const canonicalName = match[3].trim();
  if (!canonicalName) {
    throw new Error(`Missing WDSF official person name in: ${official.Name}`);
  }

  const personExternalId = linkedId(official, PERSON_REL, 'person');
  return {
    officialExternalId: official.Id.toString(),
    competitionExternalId: linkedId(official, COMPETITION_REL, 'competition'),
    personExternalId,
    personId: `wdsf:${personExternalId}`,
    roleText,
    role: mapWdsfOfficialRole(roleText),
    judgeLabel: roleText === 'Adjudicator' ? label : null,
    canonicalName,
    country: official.country,
  };
}

function linkedId(official: Official, rel: string, entity: string) {
  const href = official.link.find((link) => link.rel === rel)?.href;
  const match = href?.match(new RegExp(`/${entity}/([0-9]+)(?:$|[?#])`));
  if (!match) {
    throw new Error(`Missing WDSF ${entity} link for official ${official.Id}`);
  }
  return match[1];
}

function mapWdsfOfficialRole(role: WdsfOfficialRole): official_role | null {
  switch (role) {
    case 'Adjudicator':
      return 'adjudicator';
    case 'Chairperson':
      return 'chairperson';
    case 'Scrutiny':
      return 'scrutineer';
    case 'Invigilator':
      return 'invigilator';
    case 'DJ':
    case 'Head Judge':
      return null;
  }
}
