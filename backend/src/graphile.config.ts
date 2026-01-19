import 'graphile-config';
import path from 'path';
import { PostGraphileAmberPreset } from 'postgraphile/presets/amber';
import { makeV4Preset } from 'postgraphile/presets/v4';
import { pool } from './db.ts';
import { PgSimplifyInflectionPreset } from '@graphile/simplify-inflection';
import 'postgraphile/grafserv/express/v4';
import filePlugin from './plugins/file.ts';
import currentUserPlugin from './plugins/current-user.ts';
import { makePgService } from 'postgraphile/@dataplan/pg/adaptors/pg';
import { JWT_SECRET } from './auth.ts';
//import { OTELPlugin } from './postgraphile-otel.ts';

const isDevelopment = process.env.NODE_ENV === 'development';

const preset: GraphileConfig.Preset = {
  extends: [
    PostGraphileAmberPreset,
    PgSimplifyInflectionPreset,
    makeV4Preset({
      extendedErrors: ['hint', 'detail', 'errcode', 'where'],
      showErrorStack: 'json',
    }),
  ],

  disablePlugins: ['NodePlugin'],
  plugins: [
    ...filePlugin,
    currentUserPlugin,
    //OTELPlugin
  ],

  grafast: {
    async context(ctx) {
      const { pgSettings } = ctx.expressv4?.req ?? {};
      return { pgSettings };
    },
    explain: isDevelopment,
  },
  grafserv: {
    port: Number.parseInt(process.env.PORT || '5000', 10),
    watch: !process.env.POSTGRAPHILE_DONT_WATCH,
    graphiql: isDevelopment,
  },
  gather: {
    pgJwtTypes: ['public.jwt_token'],
    installWatchFixtures: true,
  },
  schema: {
    pgJwtSecret: JWT_SECRET,
    pgJwtSignOptions: {
      algorithm: 'HS256',
    },
    pgForbidSetofFunctionsToReturnNull: true,
    retryOnInitFail: true,
    sortExport: true,
    exportSchemaSDLPath: isDevelopment ? path.resolve('../schema.graphql') : undefined,
  },
  pgServices: [
    makePgService({
      name: 'main',
      schemas: ['public'],
      pool,
      superuserPool: pool,
    }),
  ],
};

export default preset;
