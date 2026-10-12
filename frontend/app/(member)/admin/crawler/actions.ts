'use server';

import { getRequestContext } from '@/lib/server/tenant';
import { runQuery } from '@/lib/server/postgresql';
import { crawlerOverview, crawlerFailures } from './crawler.queries';

export async function getOverview(federation: string, kind: string) {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) throw new Error('Access denied');
  const params = { federation: federation || null, kind: kind || null };
  const [rows, failures] = await Promise.all([
    runQuery(crawlerOverview, {
      ...params,
      allowRefetch: process.env.CRAWLER_DISABLE_REFETCH !== 'true',
    }),
    runQuery(crawlerFailures, params),
  ]);
  return { rows, failures };
}
