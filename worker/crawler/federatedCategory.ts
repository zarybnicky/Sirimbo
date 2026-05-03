import type { PoolClient } from 'pg';
import {
  getAllCategories,
  type IUpsertCategoryParams,
  upsertCategory,
} from './federated.queries.ts';

let categoryCache = new Map<string, string>();
let cacheTime = 0;
const CACHE_TTL_MS = 1000;

function categoryKey(params: IUpsertCategoryParams): string {
  return [
    params.series ?? '',
    params.discipline ?? '',
    params.ageGroup ?? '',
    params.genderGroup ?? '',
    params.class ?? '',
    params.competitorType ?? '',
  ].join('\0');
}

async function refreshCache(client: PoolClient): Promise<void> {
  const rows = await getAllCategories.run(undefined, client);
  const fresh = new Map<string, string>();
  for (const row of rows) {
    fresh.set(categoryKey(row), row.id);
  }
  categoryCache = fresh;
  cacheTime = Date.now();
}

export async function getFederatedCategoryId(
  client: PoolClient,
  params: IUpsertCategoryParams,
): Promise<string | undefined> {
  const key = categoryKey(params);

  if (Date.now() - cacheTime > CACHE_TTL_MS) {
    await refreshCache(client);
  }

  const cached = categoryCache.get(key);
  if (cached) return cached;

  // Cache miss — new category, upsert and add to cache
  const [result] = await upsertCategory.run(params, client);
  if (result?.id) {
    categoryCache.set(key, result.id);
    return result.id;
  }

  return undefined;
}
