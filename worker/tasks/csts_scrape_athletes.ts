import type { Task } from 'graphile-worker';
import { synchronizeAthletes } from '@rozpisovnik/csts';

const task: Task<"csts_scrape_athletes"> = async (payload = {}, workerUtils) => {
  const result = await workerUtils.withPgClient(async (client) => {
    return await synchronizeAthletes(client, {
      signal: workerUtils.abortSignal,
      onFetchError(idt, error) {
        console.error(idt, error);
      },
      cacheMaxAgeMs: 1000 * 60 * 60 * 24, // 1 day
      maxRequests: 1000,
      ...payload,
    });
  });

  await workerUtils.addJob("csts_scrape_athletes", result, {
    jobKey: 'csts_scrape_athletes',
    runAt: new Date(Date.now() + 1000 * 60),
  });
};

export default task;
