import { z } from 'zod';
import { cstsAthlete } from './cstsAthlete.ts';
import type { HtmlLoader, JsonLoader } from './types.ts';

const wdsfEvent: HtmlLoader = {
  mode: 'text',
  buildRequest: ({ key }) => ({
    url: `https://services.worlddancesport.com/api/1/events/${key}`,
  }),
  revalidatePeriod: '1 day',
  async load(url, event) {},
};

export const LOADERS: Record<string, Record<string, JsonLoader | HtmlLoader>> = {
  wdsf: {
    event: wdsfEvent,
  },
  csts: {
    member: cstsAthlete,
  },
};
