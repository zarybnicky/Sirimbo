import type { Task } from 'graphile-worker';
import {
  getScheduleStatus,
  getScheduledFetchSlots,
  getNextPendingProcess,
  getOutstandingJobCountForTask,
  getPendingFetch,
  upsertFrontiers,
} from '../crawler/crawler.queries.ts';
import { LOADERS, loaderFor, type LoaderIds } from '../crawler/handlers.ts';

const MAX_OUTSTANDING_FETCH = 100;

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

const SEED_FRONTIERS: LoaderIds[] = [
  { federation: 'wdsf', kind: 'memberIndex' },

  { federation: 'csts', kind: 'ranklistIndex' },
  { federation: 'csts', kind: 'clubIndex' },
  { federation: 'csts', kind: 'divisionIndex' },
  { federation: 'csts', kind: 'trainerIndex' },
  { federation: 'csts', kind: 'judgeIndex' },
  { federation: 'csts', kind: 'officialIndex' },

  { federation: 'szts', kind: 'competitionIndex' },
  { federation: 'szts', kind: 'memberIndex' },
  { federation: 'szts', kind: 'trainerIndex' },
  { federation: 'szts', kind: 'officialIndex' },
  { federation: 'szts', kind: 'scrutineerIndex' },
  { federation: 'szts', kind: 'judgeIndex' },
  { federation: 'szts', kind: 'clubIndex' },
  { federation: 'szts', kind: 'soloIndex' },
  { federation: 'szts', kind: 'coupleIndex' },
];

const seedFrontierParams = {
  federations: SEED_FRONTIERS.map((frontier) => frontier.federation),
  kinds: SEED_FRONTIERS.map((frontier) => frontier.kind),
  keys: SEED_FRONTIERS.map(() => ''),
};

const loaderFrontiers = Object.entries(LOADERS).flatMap(([federation, loaders]) =>
  Object.keys(loaders).map((kind) => ({ federation, kind })),
);

const fetchLoaderParams = {
  loaderFederations: loaderFrontiers.map((frontier) => frontier.federation),
  loaderKinds: loaderFrontiers.map((frontier) => frontier.kind),
};

export const frontier_schedule: Task<'frontier_schedule'> = async (_payload, helpers) => {
  const { withPgClient, addJob, logger } = helpers;

  const allowRefetch = process.env.CRAWLER_DISABLE_REFETCH !== 'true';

  await withPgClient(async (client) => {
    await upsertFrontiers.run(seedFrontierParams, client);

    const scheduleRows = await getScheduleStatus.run(undefined, client);
    const nowMs = Date.now();

    const outstanding = scheduleRows.reduce((total, row) => total + row.queued + row.locked, 0);
    if (outstanding >= MAX_OUTSTANDING_FETCH) return;

    const capacity = MAX_OUTSTANDING_FETCH - outstanding;
    const earliestRunAt = new Map<string, number>();
    const spacingMs = new Map<string, number>();
    const scheduledSlots = new Map<string, number[]>();
    for (const row of scheduleRows) {
      earliestRunAt.set(
        row.host,
        Math.max(nowMs, row.next_available_at?.getTime() ?? nowMs),
      );
      spacingMs.set(row.host, row.spacing ?? 50);
    }
    for (const row of await getScheduledFetchSlots.run(undefined, client)) {
      const slots = scheduledSlots.get(row.host) ?? [];
      slots.push(row.run_at.getTime());
      scheduledSlots.set(row.host, slots);
    }

    const pendingIds = await getPendingFetch.run(
      { capacity, allowRefetch, ...fetchLoaderParams },
      client,
    );
    let scheduled = 0;
    for (const { id, federation, kind, key } of pendingIds) {
      const loader = loaderFor(federation, kind);
      if (!loader) continue;
      const { host } = loader.buildRequest(key).url;
      const slots = scheduledSlots.get(host) ?? [];
      scheduledSlots.set(host, slots);
      const runAt = reserveEarliestSlot(
        slots,
        earliestRunAt.get(host) ?? nowMs,
        spacingMs.get(host) ?? 50,
      );
      const jobKey = `fetch:${host}:${id}`;
      const jobKeyMode = 'preserve_run_at';
      await addJob('frontier_fetch', { id }, { jobKey, jobKeyMode, runAt });
      scheduled++;
    }
    if (pendingIds.length > 0 || outstanding > 0) {
      logger.info(`${scheduled} new fetch jobs, outstanding ${outstanding}`);
    }
  });

  await withPgClient(async (client) => {
    const [workers] = await getOutstandingJobCountForTask.run({ task: 'frontier_process' }, client);
    const outstandingWorkers = workers.count;
    if (outstandingWorkers > 0) return;

    const pendingItems = await getNextPendingProcess.run({ limit: 1 }, client);
    if (pendingItems.length > 0) {
      await addJob('frontier_process', { isFullRebuild: false });
      logger.info(`Scheduled processing`);
    }
  });
};

export default frontier_schedule;
