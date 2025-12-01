import type { Task } from 'graphile-worker';
import {
  getJobCountForTask,
  getPendingFetch,
  getPendingProcess,
} from '../crawler/crawler.queries.ts';

const MAX_OUTSTANDING = 200;

export const frontier_schedule: Task<'frontier_schedule'> = async (_payload, helpers) => {
  const { withPgClient, addJob, logger } = helpers;

  await withPgClient(async (client) => {
    const countRows = await getJobCountForTask.run({ task: 'frontier_fetch' }, client);
    const outstanding = countRows[0]?.count ?? 0;
    const capacity = MAX_OUTSTANDING - outstanding;
    if (outstanding >= MAX_OUTSTANDING || capacity <= 0) return;

    const pendingIds = await getPendingFetch.run({ limit: capacity }, client);
    for (const { id } of pendingIds) {
      await addJob('frontier_fetch', { id }, { jobKey: `fetch:${id}` });
    }
    logger.info(`${pendingIds.length} new fetch jobs, outstanding ${outstanding}`);
  });

  await withPgClient(async (client) => {
    const countRows = await getJobCountForTask.run({ task: 'frontier_process' }, client);
    const outstanding = countRows[0]?.count ?? 0;
    const capacity = MAX_OUTSTANDING - outstanding;
    if (outstanding >= MAX_OUTSTANDING || capacity <= 0) return;

    const pendingIds = await getPendingProcess.run({ limit: capacity }, client);
    for (const { id } of pendingIds) {
      await addJob('frontier_process', { id }, { jobKey: `process:${id}` });
    }
    logger.info(`${pendingIds.length} new process jobs, outstanding ${outstanding}`);
  });
};

export default frontier_schedule;
