import type { PoolClient } from 'pg';
import { type IUpsertCategoryParams, upsertCategory } from './federated.queries.ts';

const categoryIdCache = new Map<string, string>();

export async function getFederatedCategoryId(
  client: PoolClient,
  params: IUpsertCategoryParams,
) {
  const key = [
    params.series ?? '',
    params.discipline ?? '',
    params.ageGroup ?? '',
    params.genderGroup ?? '',
    params.class ?? '',
    params.competitorType ?? '',
  ].join('\u0000');
  const cachedCategoryId = categoryIdCache.get(key);
  if (cachedCategoryId) return cachedCategoryId;

  const [{ id }] = await upsertCategory.run(params, client);
  if (id) categoryIdCache.set(key, id);
  return id;
}
