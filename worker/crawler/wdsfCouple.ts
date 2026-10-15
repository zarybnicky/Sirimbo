import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import {
  ensurePeople,
  mergeCompetitorComponents,
  upsertCompetitorsDetailed,
} from './federated.queries.ts';

const schema = z.object({
  id: z.string().regex(/^(rls|wdsf)-\d+$/),
  name: z.string(),
  country: z.string().nullable(),
  age: z.string().nullable(),
  division: z.string(),
  status: z.string(),
  nationalReference: z.string().nullish(),
  retireOn: z.iso.date().nullish(),
  wrlBLockedUntil: z.iso.date().nullish(),
  cupOrChampionshipBlockedUntil: z.iso.date().nullish(),
  man: z.number().int().nonnegative().nullish(),
  woman: z.number().int().nonnegative().nullish(),
  customManName: z.string().nullish(),
  customWomanName: z.string().nullish(),
  link: z.array(z.object({
    href: z.string(),
    rel: z.string(),
    type: z.string().optional(),
  })),
});

export const wdsfCouple: JsonLoader<z.output<typeof schema>> = {
  mode: 'json',
  schema,
  revalidatePeriod: '7d',
  buildRequest: (key) => ({
    url: new URL(`https://services.worlddancesport.org/api/1/couple/${key}`),
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
  async load(client, couple) {
    const id = `wdsf:${couple.id}`;
    const members = ([
      { min: couple.man, role: 'lead' },
      { min: couple.woman, role: 'follow' },
    ] as const).flatMap(({ min, role }) => min ? [{ key: min.toString(), role }] : []);

    await upsertCompetitorsDetailed.run({
      federation: ['wdsf'],
      externalId: [couple.id],
      type: ['couple'],
      label: [couple.name],
    }, client);
    if (members.length) {
      await ensurePeople.run({
        federation: members.map(() => 'wdsf'),
        externalId: members.map(({ key }) => key),
        canonicalName: members.map(() => ''),
        gender: members.map(() => 'unknown'),
      }, client);
    }
    await mergeCompetitorComponents.run({
      scopeCompetitorId: [id],
      competitorId: members.map(() => id),
      personId: members.map(({ key }) => `wdsf:${key}`),
      role: members.map(({ role }) => role),
    }, client);
    return {
      upsertFrontier: members.map(({ key }) => ({ federation: 'wdsf', kind: 'member', key })),
    };
  },
};
