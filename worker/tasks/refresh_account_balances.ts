import type { Task } from 'graphile-worker';

export const task: Task<'refresh_account_balances'> = async (_args, workerUtils) => {
  await workerUtils.withPgClient(async (pgClient) => {
    await pgClient.query(`REFRESH MATERIALIZED VIEW account_balances;`);
  });
};

export default task;
