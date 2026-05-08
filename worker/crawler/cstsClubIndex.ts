import * as cheerio from 'cheerio';
import type { HtmlLoader } from './types.ts';
import { upsertFederationClubs } from './federated.queries.ts';
import { makePgtypedCollection } from './pgtypedCollection.ts';

export const cstsClubIndex: HtmlLoader = {
  mode: 'text',
  revalidatePeriod: '1 day',
  buildRequest: () => ({ url: new URL('https://www.csts.cz/cs/Kluby') }),
  async load(client, html) {
    const clubs = makePgtypedCollection<{
      federation: string;
      externalId: string;
      name: string;
      city: string;
      country: string;
    }>(['federation', 'externalId', 'name', 'city', 'country'], ['federation', 'externalId']);

    for (const club of parseClubIndex(html)) {
      clubs.add({
        federation: 'csts',
        externalId: club.externalId,
        name: club.name,
        city: '',
        country: 'Czechia',
      });
    }

    if (clubs.length) await upsertFederationClubs.run(clubs.params, client);
  },
};

function parseClubIndex(html: string) {
  const clubs: Array<{ externalId: string; name: string }> = [];
  const $ = cheerio.load(html);

  $('table.tab1 tbody a[href^="/cs/Kluby/Detail/"]').each((_, link) => {
    const href = $(link).attr('href') ?? '';
    const externalId = /^\/cs\/Kluby\/Detail\/(\d+)$/.exec(href)?.[1];
    const name = $(link).text().trim();

    if (externalId && name) clubs.push({ externalId, name });
  });

  return clubs;
}
