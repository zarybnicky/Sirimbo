import type { Task } from 'graphile-worker';
import { synchronizeAthletes } from '@rozpisovnik/csts';

const task: Task<"csts_scrape_athletes"> = async (payload = {}, workerUtils) => {
  const { startFrom = 0 } = payload;

  const lastIdt = await workerUtils.withPgClient(async (client) => {
    return await synchronizeAthletes(client, {
      onFetchError(idt, error) {
        console.error(idt, error);
      },
      cacheMaxAgeMs: 1000 * 60 * 60 * 24, // 1 day
      maxRequests: 1000,
      startFromIdt: startFrom,
    });
  });

  await workerUtils.addJob("csts_scrape_athletes", {
    startFrom: lastIdt,
  }, {
    jobKeyMode: 'replace',
    runAt: new Date(Date.now() + 1000 * 60),
  });
};

export default task;
