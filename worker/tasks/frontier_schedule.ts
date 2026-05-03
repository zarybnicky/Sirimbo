import type { Task } from 'graphile-worker';
import {
  getFetchScheduleRules,
  getJobCountForTask,
  getNextPendingProcess,
  getPendingFetch,
} from '../crawler/crawler.queries.ts';
import { loaderFor } from '../crawler/handlers.ts';

const MAX_OUTSTANDING_FETCH = 100;

export const frontier_schedule: Task<'frontier_schedule'> = async (_payload, helpers) => {
  const { withPgClient, addJob, logger } = helpers;

  const allowRefetch = process.env.CRAWLER_DISABLE_REFETCH !== 'true';

  await withPgClient(async (client) => {
    const scheduleRows = await getFetchScheduleRules.run(undefined, client);
    const byHost = new Map<
      string,
      { spacingMs: number; queued: number; lastRunAt: Date }
    >();

    for (const r of scheduleRows) {
      if (!r.host) continue;
      byHost.set(r.host, {
        spacingMs: r.spacing ?? 50,
        queued: r.queued ?? 0,
        lastRunAt: r.last_run_at || new Date(),
      });
    }

    const outstanding = scheduleRows.reduce((acc, rule) => acc + (rule.queued ?? 0), 0);
    if (outstanding >= MAX_OUTSTANDING_FETCH) return;

    const capacity = MAX_OUTSTANDING_FETCH - outstanding;

    const pendingIds = await getPendingFetch.run({ capacity, allowRefetch }, client);
    for (const { id, federation, kind, key } of pendingIds) {
      const loader = loaderFor(federation, kind);
      if (!loader) continue;
      const { host } = loader.buildRequest(key).url;

      const rule =
        byHost.get(host) ??
        byHost.set(host, { spacingMs: 50, queued: 0, lastRunAt: new Date() }).get(host)!;
      const runAt = new Date(rule.lastRunAt.getTime() + rule.spacingMs);
      rule.lastRunAt = runAt;
      rule.queued += 1;

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
    const [workers] = await getJobCountForTask.run({ task: 'frontier_process' }, client);
    const outstandingWorkers = workers?.count ?? 0;
    if (outstandingWorkers > 0) return;

    const pendingItems = await getNextPendingProcess.run({ limit: 1 }, client);
    if (pendingItems.length > 0) {
      await addJob('frontier_process', { isFullRebuild: false });
      logger.info(`Scheduled processing`);
    }
  });
};

export default frontier_schedule;
