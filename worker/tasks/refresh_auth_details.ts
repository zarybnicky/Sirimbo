import type { Task } from 'graphile-worker';

export const task: Task<"refresh_auth_details"> = async (_args, workerUtils) => {
  await workerUtils.withPgClient(async (pgClient) => {
    await pgClient.query(`refresh materialized view concurrently auth_details;`);
  });
};

export default task;
