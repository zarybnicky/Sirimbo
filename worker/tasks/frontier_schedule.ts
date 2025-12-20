import type { Task } from 'graphile-worker';
import {
  getFetchScheduleRules,
  getJobCountForTask,
  getPendingFetch,
  countPendingProcess,
} from '../crawler/crawler.queries.ts';
import { LOADER_MAP } from '../crawler/handlers.ts';

const MAX_OUTSTANDING_FETCH = 100;

export const frontier_schedule: Task<'frontier_schedule'> = async (_payload, helpers) => {
  const { withPgClient, addJob, logger } = helpers;

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
    const capacity = MAX_OUTSTANDING_FETCH - outstanding;
    if (outstanding >= MAX_OUTSTANDING_FETCH || capacity <= 0) return;

    const pendingIds = await getPendingFetch.run({ limit: capacity }, client);
    for (const { id, federation, kind, key } of pendingIds) {
      const { url } = LOADER_MAP[federation][kind].buildRequest(key);
      const { host } = url;

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

    const [countItems] = await countPendingProcess.run(undefined, client);
    const pendingItems = countItems?.count ?? 0;
    if (pendingItems <= 0) return;

    await addJob('frontier_process', {});
    logger.info(`Scheduled 1 process task, pending ${pendingItems} items`);
  });
};

export default frontier_schedule;
