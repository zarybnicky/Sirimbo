import type { Task } from 'graphile-worker';

export const task: Task<"create_lesson_payments"> = async (_args, workerUtils) => {
  await workerUtils.withPgClient(async (pgClient) => {
    await pgClient.query(`select app_private.create_latest_lesson_payments()`);
  });
};

export default task;
