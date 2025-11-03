import { discoverAthletes, refreshAthletes } from './index.ts';
import { Pool } from 'pg';

const pool = new Pool();
pool.connect().then(async (client) => {
  await discoverAthletes(client, {
    onFetchError(idt, error) {
      console.error('[discover]', idt, error);
    },
    maxRequests: 10,
  });

  await refreshAthletes(client, {
    onFetchError(idt, error) {
      console.error('[refresh]', idt, error);
    },
    maxRequests: 10,
  });
});
