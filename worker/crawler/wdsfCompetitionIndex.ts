import { z } from 'zod';
import { type JsonLoader } from './types.ts';
import { endOfMonth } from 'date-fns';

const rangeKeyRe = /^(\d{4})-(\d{2})$/;

function parseRangeKey(key: string) {
  const match = rangeKeyRe.exec(key);
  if (!match) {
    throw new Error(`Invalid wdsf competitionIndex key: ${key}`);
  }
  const month = new Date(Date.UTC(parseInt(match[1]), parseInt(match[2]) - 1, 1));
  return {
    from: month.toISOString().slice(0, 10).replace('-', '/'),
    to: endOfMonth(month).toISOString().slice(0, 10).replace('-', '/'),
  };
}

const schema = z.array(
  z.object({
    link: z.array(
      z.object({
        href: z.string(),
        rel: z.string(),
        type: z.string().optional(),
      })
    ),
    id: z.number(),
    name: z.string(),
    lastmodifiedDate: z.string(),
  }),
);

export const wdsfCompetitionIndex: JsonLoader<z.infer<typeof schema>> = {
  mode: 'json',
  schema,
  buildRequest: (key) => {
    const { from, to } = parseRangeKey(key);
    return {
      url: new URL(`https://services.worlddancesport.org/api/1/competition?from=${from}&to=${to}`),
      init: {
        headers: {
          Authorization: process.env.WDSF_AUTH || '',
          Accept: 'application/json',
        },
      },
    };
  },
  revalidatePeriod: '12h',
  async load(_, parsed) {
    return {
      upsertFrontier: parsed.map((x) => ({
        federation: 'wdsf',
        kind: 'competition',
        key: x.id.toString(),
      })),
    };
  },
};
