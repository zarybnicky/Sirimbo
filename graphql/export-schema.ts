import { makePgService } from 'postgraphile/adaptors/pg';
import { Pool } from 'pg';
import { postgraphile } from 'postgraphile';
import sharedPreset from './graphile.config.ts';

const pool = new Pool();
const instance = postgraphile({
  extends: [sharedPreset],
  pgServices: [makePgService({ schemas: ['public', 'federated', 'crawler'], pool, superuserPool: pool })],
  grafserv: { watch: false },
  gather: { installWatchFixtures: false },
  schema: { retryOnInitFail: false, exportSchemaSDLPath: process.argv[2] },
});
try {
  await instance.getSchema();
} finally {
  await instance.release();
  await pool.end();
}
