import type { AddJobsJobSpec, Helpers, Task } from 'graphile-worker';
import type { PoolClient } from 'pg';
import {
  getScheduleStatus,
  getPendingFetch,
  queuePendingProcess,
  upsertFrontiers,
} from '../crawler/crawler.queries.ts';
import { ALL_LOADERS, loaderFor, SEED_FRONTIERS } from '../crawler/handlers.ts';

const MAX_OUTSTANDING_FETCH = 200;

function reserveEarliestSlot(
  sortedSlots: number[],
  earliest: number,
  spacing: number,
): Date {
  let candidate = earliest;
  let insertAt = 0;

  for (; insertAt < sortedSlots.length; insertAt++) {
    const slot = sortedSlots[insertAt];
    if (slot < candidate) {
      if (candidate - slot < spacing) candidate = slot + spacing;
    } else if (slot - candidate < spacing) {
      candidate = slot + spacing;
    } else {
      break;
    }
  }

  sortedSlots.splice(insertAt, 0, candidate);
  return new Date(candidate);
}

const seedFrontierParams = {
  federations: SEED_FRONTIERS.map((frontier) => frontier.federation),
  kinds: SEED_FRONTIERS.map((frontier) => frontier.kind),
  keys: SEED_FRONTIERS.map(() => ''),
  referenceDates: SEED_FRONTIERS.map(() => ''),
};

const fetchLoaderParams = {
  loaderFederations: ALL_LOADERS.map((f) => f.federation),
  loaderKinds: ALL_LOADERS.map((f) => f.kind),
};

export const frontier_schedule: Task<'frontier_schedule'> = async (_payload, helpers) => {
  await helpers.withPgClient(async (client) => {
    await upsertFrontiers.run(seedFrontierParams, client);
    await queuePendingProcess.run(undefined, client);
    await scheduleFetches(client, helpers);
  });
};

export async function scheduleFetches(client: PoolClient, { addJobs, logger }: Helpers) {
  const allowRefetch = process.env.CRAWLER_DISABLE_REFETCH !== 'true';

  const scheduleRows = await getScheduleStatus.run(undefined, client);
  const nowMs = Date.now();

  const outstanding = scheduleRows.reduce((total, row) => total + row.queued + row.locked, 0);
  if (outstanding >= MAX_OUTSTANDING_FETCH) return;

  const capacity = MAX_OUTSTANDING_FETCH - outstanding;
  const earliest = new Map<string, number>();
  const spacing = new Map<string, number>();
  const scheduledSlots = new Map<string, number[]>();
  for (const row of scheduleRows) {
    earliest.set(
      row.host,
      Math.max(nowMs, row.next_available_at?.getTime() ?? nowMs),
    );
    spacing.set(row.host, row.spacing ?? 50);
    scheduledSlots.set(row.host, row.slots.map((slot) => slot.getTime()));
  }

  const pendingIds = await getPendingFetch.run(
    { capacity, allowRefetch, ...fetchLoaderParams },
    client,
  );
  const jobs: AddJobsJobSpec<'frontier_fetch'>[] = [];
  for (const { id, federation, kind, key } of pendingIds) {
    const loader = loaderFor(federation, kind);
    if (!loader) continue;
    const { host } = loader.buildRequest(key).url;
    const slots = scheduledSlots.get(host) ?? [];
    scheduledSlots.set(host, slots);
    jobs.push({
      identifier: 'frontier_fetch',
      payload: { id },
      jobKey: `fetch:${host}:${id}`,
      runAt: reserveEarliestSlot(slots, earliest.get(host) ?? nowMs, spacing.get(host) ?? 50),
    });
  }
  if (jobs.length > 0) await addJobs(jobs, true);
  if (pendingIds.length > 0 || outstanding > 0) {
    logger.info(`${jobs.length} new fetch jobs, outstanding ${outstanding}`);
  }
}

export default frontier_schedule;
