import { makePgService } from 'postgraphile/adaptors/pg';
import { PgSimplifyInflectionPreset } from '@graphile/simplify-inflection';
import 'postgraphile/grafserv/express/v4';
import 'graphile-config';
import path from 'path';
import { PostGraphileAmberPreset } from 'postgraphile/presets/amber';
import { QueryControlsPlugin } from './plugins/query-controls.ts';
import { JWT_SECRET } from './auth.ts';
import { pool } from './db.ts';
import currentUserPlugin from './plugins/current-user.ts';
import { FailedLoginPlugin } from './plugins/failed-login.ts';
import { LegacyRelationAliasesPlugin, LegacyShimsPlugin } from './plugins/legacy-shims.ts';
import federationConditionPlugins from './plugins/federation-condition.ts';
import competitionParticipantConditionPlugins from './plugins/competition-participant-condition.ts';
import competitionSuggestionConditionPlugins from './plugins/competition-suggestion-condition.ts';
import federationOrderPlugin from './plugins/federation-order.ts';
import personMembershipConditionPlugin from './plugins/person-membership-condition.ts';
import coupleConditionPlugins from './plugins/couple-condition.ts';
import eventConditionPlugins from './plugins/event-condition.ts';
import transactionConditionPlugins from './plugins/transaction-condition.ts';
import { EventDetailsPlugin } from './plugins/event-details.ts';
import { OTELPlugin } from './postgraphile-otel.ts';
import { ApiInflectionPlugin } from './plugins/api-inflection.ts';

const isDevelopment = process.env.NODE_ENV === 'development';

const preset: GraphileConfig.Preset = {
  extends: [PostGraphileAmberPreset, PgSimplifyInflectionPreset],

  disablePlugins: [
    'NodePlugin',
    'QueryQueryPlugin',
    'MutationPayloadQueryPlugin',
    'PgIndexBehaviorsPlugin',
  ],
  plugins: [
    ApiInflectionPlugin,
    currentUserPlugin,
    ...personMembershipConditionPlugin,
    ...coupleConditionPlugins,
    ...eventConditionPlugins,
    ...transactionConditionPlugins,
    EventDetailsPlugin,
    ...federationConditionPlugins,
    ...competitionParticipantConditionPlugins,
    ...competitionSuggestionConditionPlugins,
    federationOrderPlugin,
    FailedLoginPlugin,
    LegacyShimsPlugin,
    LegacyRelationAliasesPlugin,
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

export const standalonePreset: GraphileConfig.Preset = {
  extends: [preset],
  grafserv: { watch: false, graphiql: false },
  gather: { installWatchFixtures: false },
  schema: {
    exportSchemaSDLPath: undefined,
    exportSchemaIntrospectionResultPath: undefined,
    retryOnInitFail: false,
  },
};

export default preset;
