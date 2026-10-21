import { makePgService } from 'postgraphile/adaptors/pg';
import { PgSimplifyInflectionPreset } from '@graphile/simplify-inflection';
import 'postgraphile/grafserv/express/v4';
import 'graphile-config';
import path from 'path';
import { PostGraphileAmberPreset } from 'postgraphile/presets/amber';
import { makeV4Preset } from 'postgraphile/presets/v4';
import { QueryControlsPlugin } from './plugins/query-controls.ts';
import { JWT_SECRET } from './auth.ts';
import { pool } from './db.ts';
import currentUserPlugin from './plugins/current-user.ts';
import { FailedLoginPlugin } from './plugins/failed-login.ts';
import { LegacyShimsPlugin } from './plugins/legacy-shims.ts';
import federationConditionPlugins from './plugins/federation-condition.ts';
import competitionParticipantConditionPlugins from './plugins/competition-participant-condition.ts';
import federationOrderPlugin from './plugins/federation-order.ts';
import personMembershipConditionPlugin from './plugins/person-membership-condition.ts';
import { OTELPlugin } from './postgraphile-otel.ts';
import { ApiInflectionPlugin } from './plugins/api-inflection.ts';

const isDevelopment = process.env.NODE_ENV === 'development';

const preset: GraphileConfig.Preset = {
  extends: [
    PostGraphileAmberPreset,
    PgSimplifyInflectionPreset,
    makeV4Preset({ ignoreRBAC: false }),
  ],

  disablePlugins: [
    'NodePlugin',
    'QueryQueryPlugin',
    'MutationPayloadQueryPlugin',
    'PgV4BuildPlugin',
    'PgV4BehaviorPlugin',
    'PgV4InflectionPlugin',
  ],
  plugins: [
    ApiInflectionPlugin,
    currentUserPlugin,
    ...personMembershipConditionPlugin,
    ...federationConditionPlugins,
    ...competitionParticipantConditionPlugins,
    federationOrderPlugin,
    FailedLoginPlugin,
    LegacyShimsPlugin,
    OTELPlugin,
    QueryControlsPlugin,
  ],

  grafast: {
    async context(ctx) {
      const { pgSettings } = ctx.expressv4?.req ?? {};
      return { pgSettings };
    },
    explain: isDevelopment,
  },
  grafserv: {
    port: Number.parseInt(process.env.PORT || '5200', 10),
    watch: !process.env.POSTGRAPHILE_DONT_WATCH,
    graphiql: isDevelopment,
    maskError: (error) => error,
  },
  gather: {
    pgJwtTypes: ['public.jwt_token'],
    pgFakeConstraintsAutofixForeignKeyUniqueness: false,
    installWatchFixtures: true,
  },
  schema: {
    dontSwallowErrors: true,
    jsonScalarAsString: false,
    pgMutationPayloadRelations: false,
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
      schemas: ['public', 'federated', 'crawler'],
      pool,
      superuserPool: pool,
    }),
  ],
};

export default preset;
