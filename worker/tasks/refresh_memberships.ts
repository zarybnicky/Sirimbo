import type { Task } from 'graphile-worker';

export const task: Task<'refresh_memberships'> = async (_args, workerUtils) => {
  await workerUtils.withPgClient(async (pgClient) => {
    await pgClient.query(`select app_private.cron_update_memberships();`);
  });
};

export default task;
