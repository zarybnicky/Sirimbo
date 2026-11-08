'use server';

import { z } from 'zod';
import { runQuery, withTransaction } from '@/lib/server/postgresql';
import { getRequestContext } from '@/lib/server/tenant';
import { revalidatePath } from 'next/cache';
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

const policyEditSchema = z
  .object({
    federation: z.string().min(1),
    kind: z.string().min(1),
    offset: z.string().nullable(),
    fetchInterval: z.string(),
    field: z.enum(['offset', 'fetchInterval']),
    amount: z.int(),
    unit: z.enum(['seconds', 'minutes', 'hours', 'days', 'weeks', 'months', 'years']),
  })
  .refine(
    (edit) => (edit.field === 'offset' ? edit.offset !== null : edit.amount > 0),
    'Fetch intervals must be positive; default policies have no boundary.',
  );

export async function updateRefetchPolicy(input: z.input<typeof policyEditSchema>) {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) throw new Error('FORBIDDEN');
  const edit = policyEditSchema.parse(input);
  await withTransaction(async (client) => {
    const result = await client
      .query(
        `UPDATE crawler.refetch_policy
       SET reference_offset = CASE WHEN $5 = 'offset' THEN $6::interval ELSE reference_offset END,
           fetch_interval = CASE WHEN $5 = 'fetchInterval' THEN $6::interval ELSE fetch_interval END
       WHERE federation = $1 AND kind = $2
         AND reference_offset IS NOT DISTINCT FROM $3::interval
         AND fetch_interval = $4::interval
       RETURNING federation`,
        [
          edit.federation,
          edit.kind,
          edit.offset,
          edit.fetchInterval,
          edit.field,
          `${edit.amount} ${edit.unit}`,
        ],
      )
      .catch((error: unknown) => {
        if (
          error &&
          typeof error === 'object' &&
          'code' in error &&
          error.code === '23505'
        ) {
          throw new Error('A policy already has this boundary. Choose another offset.');
        }
        throw error;
      });
    if (result.rowCount !== 1)
      throw new Error('This policy changed. Reload the page and try again.');
    await client.query('SELECT crawler.recalculate_fetch_times($1, $2)', [
      edit.federation,
      edit.kind,
    ]);
  });
  revalidatePath('/admin/crawler');
}

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
