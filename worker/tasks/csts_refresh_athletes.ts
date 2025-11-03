import type { Task } from 'graphile-worker';
import { refreshAthletes } from '@rozpisovnik/csts';

const DEFAULT_MAX_REQUESTS = 100;

const task: Task<'csts_refresh_athletes'> = async (payload = {}, workerUtils) => {
  const maxRequests = payload.maxRequests ?? DEFAULT_MAX_REQUESTS;

  await workerUtils.withPgClient(async (client) => {
    return await refreshAthletes(client, {
      maxRequests,
      signal: workerUtils.abortSignal,
      async onFetchError(idt, error) {
        console.error('[csts:refresh]', idt, error);
      },
    });
  });

  await workerUtils.addJob(
    'csts_refresh_athletes',
    { maxRequests },
    {
      jobKey: 'csts_refresh_athletes',
      runAt: new Date(Date.now() + 5 * 60_000),
    },
  );
};

export default task;
