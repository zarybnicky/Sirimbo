import type { PoolClient } from 'pg';
import { getAllDancePrograms, upsertDancePrograms } from './federated.queries.ts';
import { z } from 'zod';
import { makePgtypedCollection } from './pgtypedCollection.ts';

const danceCodes = {
  SW: 'SW',
  Waltz: 'SW',
  WALTZ: 'SW',
  TG: 'TG',
  Tango: 'TG',
  TANGO: 'TG',
  VW: 'VW',
  'Viennese Waltz': 'VW',
  'VIENNESE WALTZ': 'VW',
  SF: 'SF',
  Slowfox: 'SF',
  'Slow Foxtrot': 'SF',
  'SLOW FOXTROT': 'SF',
  QS: 'QS',
  Quickstep: 'QS',
  QUICKSTEP: 'QS',
  SA: 'SA',
  Samba: 'SA',
  SAMBA: 'SA',
  CH: 'CH',
  Chachacha: 'CH',
  ChaChaCha: 'CH',
  'Cha Cha': 'CH',
  'CHA CHA': 'CH',
  'CHA CHA CHA': 'CH',
  RU: 'RU',
  Rumba: 'RU',
  RUMBA: 'RU',
  PD: 'PD',
  'Paso Doble': 'PD',
  'PASO DOBLE': 'PD',
  JI: 'JI',
  Jive: 'JI',
  JIVE: 'JI',
  PK: 'PK',
  Polka: 'PK',
  BA: 'BA',
  Bachata: 'BA',
  ME: 'ME',
  Merengue: 'ME',
  SL: 'SL',
  Salsa: 'SL',
  KRB: 'KRB',
  Karibik: 'KRB',
  OT: 'OT',
  GR: 'OT',
  Skupina: 'OT',
  Formace: 'OT',
  '?d': 'OT',
  LAT: 'OT',
  SyLAT: 'OT',
  SYLAT: 'OT',
  'FORMATION LATIN': 'OT',
  BREAKING: 'OT',
} as const;

const danceCodeKeys = Object.keys(danceCodes) as [
  keyof typeof danceCodes,
  ...(keyof typeof danceCodes)[],
];

export const danceCode = z
  .preprocess(
    (dance) => (typeof dance === 'string' ? dance.trim().replace(/^\d+\s+/, '') : dance),
    z.enum(danceCodeKeys),
  )
  .transform((dance) => danceCodes[dance]);

export type DanceCode = z.infer<typeof danceCode>;

let cache = new Map<string, string>();
let cacheTime = 0;
const CACHE_TTL_MS = 1000;

function uniqueDanceCodes(danceCodes: readonly DanceCode[]) {
  return [...new Set(danceCodes)];
}

async function refreshCache(client: PoolClient) {
  const rows = await getAllDancePrograms.run(undefined, client);
  cache = new Map(rows.flatMap((row) => (row.code ? [[row.code, row.id]] : [])));
  cacheTime = Date.now();
}

export async function getDanceProgramIds(
  client: PoolClient,
  inputCodeLists: readonly (readonly DanceCode[])[],
): Promise<string[]> {
  if (inputCodeLists.length === 0) return [];

  if (Date.now() - cacheTime > CACHE_TTL_MS) {
    await refreshCache(client);
  }

  const programs = makePgtypedCollection<{
    code: string;
    name: string;
    discipline: string;
  }>(['code', 'name', 'discipline'], ['code']);

  const programDances = makePgtypedCollection<{
    programCode: string;
    danceCode: DanceCode;
    danceOrder: number;
  }>(['programCode', 'danceCode', 'danceOrder'], ['programCode', 'danceCode']);

  for (const danceCodes of inputCodeLists.map(uniqueDanceCodes)) {
    const programCode = danceCodes.join('-');
    if (cache.has(programCode)) continue;
    programs.add({ code: programCode, name: danceCodes.join(' '), discipline: '' });
    danceCodes.forEach((danceCode, index) => {
      programDances.add({ programCode, danceCode, danceOrder: index + 1 });
    });
  }

  if (programs.length) {
    const rows = await upsertDancePrograms.run(
      {
        ...programs.params,
        ...programDances.params,
      },
      client,
    );
    for (const row of rows) {
      if (row.code) cache.set(row.code, row.id);
    }
  }

  const result: string[] = [];
  for (const danceCodes of inputCodeLists.map(uniqueDanceCodes)) {
    const programCode = cache.get(danceCodes.join('-'));
    if (!programCode) throw new Error('Failed to insert program');
    result.push(programCode);
  }
  return result;
}
