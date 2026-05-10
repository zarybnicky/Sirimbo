import * as cheerio from 'cheerio';
import { ensureCompetitors } from './federated.queries.ts';
import { makePgtypedCollection } from './pgtypedCollection.ts';
import type { competitor_type } from './federated.queries.ts';
import type { HtmlLoader } from './types.ts';

export const sztsSoloIndex = sztsCompetitorIndexLoader(
  'https://szts.ksis.eu/menu.php?akcia=CZSD',
  'solo',
  (cells) => cells[1] ?? '',
);

export const sztsCoupleIndex = sztsCompetitorIndexLoader(
  'https://szts.ksis.eu/menu.php?akcia=CZP',
  'couple',
  (cells) => [cells[1], cells[2]].filter(Boolean).join(' - '),
);

function sztsCompetitorIndexLoader(
  url: string,
  type: competitor_type,
  labelFromCells: (cells: string[]) => string,
): HtmlLoader {
  return {
    mode: 'text',
    revalidatePeriod: '1 day',
    buildRequest: () => ({ url: new URL(url) }),
    async load(client, html) {
      const competitors = makePgtypedCollection<{
        federation: string;
        externalId: string;
        type: competitor_type;
        label: string;
      }>(['federation', 'externalId', 'type', 'label'], ['federation', 'externalId']);

      for (const competitor of parseSztsCompetitorIndex(html, labelFromCells)) {
        competitors.add({
          federation: 'szts',
          externalId: competitor.externalId,
          type,
          label: competitor.label,
        });
      }

      if (competitors.length) await ensureCompetitors.run(competitors.params, client);
    },
  };
}

function parseSztsCompetitorIndex(
  html: string,
  labelFromCells: (cells: string[]) => string,
) {
  const competitors: Array<{ externalId: string; label: string }> = [];
  const $ = cheerio.load(html);

  $('table tr').each((_, row) => {
    const cells = $(row)
      .find('td')
      .toArray()
      .map((cell) => $(cell).text().trim().replace(/\s+/g, ' '));
    const externalId = cells[0] ?? '';
    const label = labelFromCells(cells);

    if (/^\d+$/.test(externalId) && label) competitors.push({ externalId, label });
  });

  return competitors;
}
