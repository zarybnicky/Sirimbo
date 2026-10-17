import type { Task } from 'graphile-worker';

export const task: Task<'refresh_event_people'> = async (_args, helpers) => {
  await helpers.withPgClient(async (client) => {
    await client.query('begin');
    try {
      await client.query("set local work_mem = '64MB'");
      await client.query('refresh materialized view concurrently app_private.event_people');
      await client.query('commit');
    } catch (error) {
      await client.query('rollback');
      throw error;
    }
  });
};

export default task;
