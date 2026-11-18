import { makePgService } from 'postgraphile/adaptors/pg';
import sharedPreset from '@rozpisovnik/graphql/graphile.config';
import path from 'node:path';
import { pool } from './db.ts';
import 'postgraphile/grafserv/express/v4';

const preset: GraphileConfig.Preset = {
  extends: [sharedPreset],
  pgServices: [
    makePgService({
      schemas: ['public', 'federated', 'crawler'],
      pool,
      superuserPool: pool,
    }),
  ],
  schema: {
    exportSchemaSDLPath:
      process.env.NODE_ENV === 'development'
        ? path.resolve(import.meta.dirname, '../../schema.graphql')
        : undefined,
  },
  grafast: { context: (ctx) => ({ pgSettings: ctx.expressv4?.req.pgSettings }) },
};
export default preset;
