import type { Task } from 'graphile-worker';
import {
  getCrawlerScheduleStatus,
  getNextPendingProcess,
  getOutstandingJobCountForTask,
  getPendingFetch,
  upsertFrontiers,
} from '../crawler/crawler.queries.ts';
import { loaderFor, type LoaderIds } from '../crawler/handlers.ts';

const MAX_OUTSTANDING_FETCH = 100;

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

export const frontier_schedule: Task<'frontier_schedule'> = async (_payload, helpers) => {
  const { withPgClient, addJob, logger } = helpers;

  const allowRefetch = process.env.CRAWLER_DISABLE_REFETCH !== 'true';

  await withPgClient(async (client) => {
    await upsertFrontiers.run(seedFrontierParams, client);

    const scheduleRows = await getCrawlerScheduleStatus.run(undefined, client);
    const byHost = new Map<
      string,
      { spacingMs: number; queueTailAt: Date }
    >();
    const now = new Date();

    for (const r of scheduleRows) {
      const queueTailAt = r.queue_tail_at && r.queue_tail_at > now ? r.queue_tail_at : now;
      byHost.set(r.host, {
        spacingMs: r.spacing ?? 50,
        queueTailAt,
      });
    }

    const outstanding = scheduleRows.reduce((acc, rule) => acc + rule.queued + rule.locked, 0);
    if (outstanding >= MAX_OUTSTANDING_FETCH) return;

    const capacity = MAX_OUTSTANDING_FETCH - outstanding;

    const pendingIds = await getPendingFetch.run({ capacity, allowRefetch }, client);
    for (const { id, federation, kind, key } of pendingIds) {
      const loader = loaderFor(federation, kind);
      if (!loader) continue;
      const { host } = loader.buildRequest(key).url;

      const rule =
        byHost.get(host) ??
        byHost.set(host, { spacingMs: 50, queueTailAt: now }).get(host)!;
      const runAt = new Date(rule.queueTailAt.getTime() + rule.spacingMs);
      rule.queueTailAt = runAt;

      const jobKey = `fetch:${host}:${id}`;
      await addJob(
        'frontier_fetch',
        { id },
        { jobKey, jobKeyMode: 'preserve_run_at', runAt },
      );
    }
    if (pendingIds.length > 0 || outstanding > 0)
      logger.info(`${pendingIds.length} new fetch jobs, outstanding ${outstanding}`);
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
