'use server';

import { z } from 'zod';
import { runQuery, withTransaction } from '@/lib/server/postgresql';
import { getRequestContext } from '@/lib/server/tenant';
import {
  completeCrawlerJob as completeCrawlerJobQuery,
  refetchCrawlerErrors,
  reprocessCrawlerTargets,
  scheduleCrawler,
} from './crawler.queries';

const actionSchema = z.enum(['refetch-errors', 'reprocess-all']);
const targetSchema = z.object({
  federation: z.string().min(1),
  kind: z.string().min(1),
  keyContains: z.string().default(''),
  failureGroup: z
    .object({
      failure: z.string().min(1),
      httpStatus: z.int().nullable(),
      errorFingerprint: z.string(),
    })
    .optional(),
});

export async function completeCrawlerJob(jobId: string) {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) throw new Error('FORBIDDEN');

  await runQuery(completeCrawlerJobQuery, {
    jobId: z
      .string()
      .regex(/^[1-9]\d{0,18}$/)
      .parse(jobId),
  });
}

export async function queueCrawlerAction(
  action: z.infer<typeof actionSchema>,
  target: z.input<typeof targetSchema>,
) {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) throw new Error('FORBIDDEN');

  const operation = actionSchema.parse(action);
  const params = targetSchema.parse(target);
  return withTransaction(async (client) => {
    const query =
      operation === 'refetch-errors' ? refetchCrawlerErrors : reprocessCrawlerTargets;
    const [result] = await query.run(
      {
        ...params,
        failure: params.failureGroup?.failure ?? null,
        httpStatus: params.failureGroup?.httpStatus ?? null,
        errorFingerprint: params.failureGroup?.errorFingerprint ?? null,
      },
      client,
    );
    const count = result!.count;
    if (count > 0) await scheduleCrawler.run(undefined, client);
    return count;
  });
}
