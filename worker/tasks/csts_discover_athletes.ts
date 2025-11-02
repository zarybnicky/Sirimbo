import type { Task } from 'graphile-worker';
import { discoverAthletes } from '@rozpisovnik/csts';

const DEFAULT_MAX_REQUESTS = 250;

const task: Task<'csts_discover_athletes'> = async (payload = {}, workerUtils) => {
  const { lastFoundIdt, lastCheckedIdt } = payload;
  const maxRequests = payload.maxRequests ?? DEFAULT_MAX_REQUESTS;

  const result = await workerUtils.withPgClient(async (client) => {
    return await discoverAthletes(client, {
      signal: workerUtils.abortSignal,
      lastFoundIdt,
      lastCheckedIdt,
      maxRequests,
      async onFetchError(idt, error) {
        console.error('[csts:discover]', idt, error);
      },
    });
  });

  await workerUtils.addJob('csts_discover_athletes', { ...result, maxRequests }, {
    jobKey: 'csts_discover_athletes',
    runAt: new Date(Date.now() + 60_000),
  });
};

export default task;
