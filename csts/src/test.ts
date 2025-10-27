import { synchronizeAthletes } from './index.ts'
import { Pool } from 'pg';

const pool = new Pool();
pool.connect().then((client) => {
  synchronizeAthletes(client, {
    onFetchError(idt, error) {
      console.error(idt, error);
    },
    cacheMaxAgeMs: 1000 * 60 * 60 * 24 // 1 day
  });
});
