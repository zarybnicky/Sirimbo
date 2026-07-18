import {
  type competitor_type,
  getAllCategories,
  upsertCategories,
} from './federated.queries.ts';
import { makePgtypedCollection } from './pgtypedCollection.ts';
import { pool } from '../pool.ts';

let categoryCache = new Map<string, string>();
let cacheTime = 0;
const CACHE_TTL_MS = 1000;

export type CategoryParams = {
  series: string;
  discipline: string;
  ageGroup: string;
  genderGroup: string;
  class: string;
  competitorType: competitor_type;
};

function categoryKey(params: CategoryParams): string {
  return [
    params.series ?? '',
    params.discipline ?? '',
    params.ageGroup ?? '',
    params.genderGroup ?? '',
    params.class ?? '',
    params.competitorType ?? '',
  ].join('\0');
}

async function refreshCache(): Promise<void> {
  const rows = await getAllCategories.run(undefined, pool);
  const fresh = new Map<string, string>();
  for (const row of rows) {
    fresh.set(categoryKey(row), row.id);
  }
  categoryCache = fresh;
  cacheTime = Date.now();
}

export async function getFederatedCategoryId(
  params: CategoryParams,
): Promise<string> {
  return (await getFederatedCategoryIds([params]))[0]!;
}

export async function getFederatedCategoryIds(
  params: readonly CategoryParams[],
): Promise<string[]> {
  if (params.length === 0) return [];

  if (Date.now() - cacheTime > CACHE_TTL_MS) {
    await refreshCache();
  }

  const misses = makePgtypedCollection<{
    series: string;
    discipline: string;
    ageGroup: string;
    genderGroup: string;
    class: string;
    competitorType: competitor_type;
  }>(
    ['series', 'discipline', 'ageGroup', 'genderGroup', 'class', 'competitorType'],
    ['series', 'discipline', 'ageGroup', 'genderGroup', 'class', 'competitorType'],
  );
  for (const item of params) {
    if (!categoryCache.has(categoryKey(item))) {
      misses.add({
        series: item.series ?? '',
        discipline: item.discipline ?? '',
        ageGroup: item.ageGroup ?? '',
        genderGroup: item.genderGroup ?? '',
        class: item.class ?? '',
        competitorType: item.competitorType,
      });
    }
  }

  if (misses.length) {
    const rows = await upsertCategories.run(misses.params, pool);
    for (const row of rows) {
      categoryCache.set(categoryKey(row), row.id);
    }

    if (params.some((item) => !categoryCache.has(categoryKey(item)))) {
      await refreshCache();
    }
  }

  return params.map((item) => categoryCache.get(categoryKey(item))!);
}
