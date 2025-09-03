
import type { Task } from 'graphile-worker';

export const task: Task<"send_invitation"> = async (_args, workerUtils) => {
  await workerUtils.withPgClient(async (pgClient) => {
    await pgClient.query(`refresh materialized view concurrently auth_details;`);
  });
};

export default task;
