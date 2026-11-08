'use server';

import { z } from 'zod';
import { runQuery, withTransaction } from '@/lib/server/postgresql';
import { getRequestContext } from '@/lib/server/tenant';
import { revalidatePath } from 'next/cache';
import {
  completeCrawlerJob as completeCrawlerJobQuery,
  createRefetchPolicy as createRefetchPolicyQuery,
  updateRefetchPolicy as updateRefetchPolicyQuery,
  removeRefetchPolicy as removeRefetchPolicyQuery,
  type IRemoveRefetchPolicyParams,
  recalculateCrawlerFetchTimes,
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

const intervalUnitSchema = z.enum([
  'seconds',
  'minutes',
  'hours',
  'days',
  'weeks',
  'months',
  'years',
]);

const policyCreateSchema = z.object({
  federation: z.string().min(1),
  kind: z.string().min(1),
  amount: z.int().positive(),
  unit: intervalUnitSchema,
  offsetAmount: z.int(),
  offsetUnit: intervalUnitSchema,
});

export async function createRefetchPolicy(input: z.input<typeof policyCreateSchema>) {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) throw new Error('FORBIDDEN');
  const policy = policyCreateSchema.parse(input);
  await withTransaction(async (client) => {
    const result = await createRefetchPolicyQuery.run(
      {
        federation: policy.federation,
        kind: policy.kind,
        offset: `${policy.offsetAmount} ${policy.offsetUnit}`,
        fetchInterval: `${policy.amount} ${policy.unit}`,
      },
      client,
    );
    if (result.length !== 1) throw new Error('This offset already exists.');
    await recalculateCrawlerFetchTimes.run(policy, client);
  });
  revalidatePath('/admin/crawler');
}

const policyEditSchema = policyCreateSchema.extend({
  offset: z.string().nullable(),
  fetchInterval: z.string(),
});

export async function updateRefetchPolicy(input: z.input<typeof policyEditSchema>) {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) throw new Error('FORBIDDEN');
  const edit = policyEditSchema.parse(input);
  await withTransaction(async (client) => {
    const result = await updateRefetchPolicyQuery
      .run(
        {
          ...edit,
          newOffset: `${edit.offsetAmount} ${edit.offsetUnit}`,
          newFetchInterval: `${edit.amount} ${edit.unit}`,
        },
        client,
      )
      .catch((error: unknown) => {
        if (
          error &&
          typeof error === 'object' &&
          'code' in error &&
          error.code === '23505'
        ) {
          throw new Error('This offset already exists.');
        }
        throw error;
      });
    if (result.length !== 1)
      throw new Error('This interval changed. Reload the page and try again.');
    await recalculateCrawlerFetchTimes.run(edit, client);
  });
  revalidatePath('/admin/crawler');
}

export async function removeRefetchPolicy(policy: IRemoveRefetchPolicyParams) {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) throw new Error('FORBIDDEN');
  await withTransaction(async (client) => {
    await removeRefetchPolicyQuery.run(policy, client);
    await recalculateCrawlerFetchTimes.run(policy, client);
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
