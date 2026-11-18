import { PgSimplifyInflectionPreset } from '@graphile/simplify-inflection';
import 'graphile-config';
import { PostGraphileAmberPreset } from 'postgraphile/presets/amber';
import { QueryControlsPlugin } from './plugins/query-controls.ts';

import currentUserPlugin from './plugins/current-user.ts';
import { LegacyRelationAliasesPlugin, LegacyShimsPlugin } from './plugins/legacy-shims.ts';
import federationConditionPlugins from './plugins/federation-condition.ts';
import competitionParticipantConditionPlugins from './plugins/competition-participant-condition.ts';
import competitionSuggestionConditionPlugins from './plugins/competition-suggestion-condition.ts';
import federationOrderPlugin from './plugins/federation-order.ts';
import { AccessCredentialFieldsPlugin } from './plugins/access-credentials.ts';
import peoplePlugins from './plugins/people.ts';
import { DateRangeConditionPlugin } from './plugins/date-range-condition.ts';
import eventConditionPlugins from './plugins/event-condition.ts';
import transactionConditionPlugins from './plugins/transaction-condition.ts';
import paymentConditionPlugins from './plugins/payment-condition.ts';
import { EventDetailsPlugin, EventFieldsPlugin } from './plugins/event-details.ts';
import { OTELPlugin } from './postgraphile-otel.ts';
import { ApiInflectionPlugin } from './plugins/api-inflection.ts';

declare global {
  namespace Grafast {
    interface RequestContext { pgSettings?: Record<string, string>; }
  }
}

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
    DateRangeConditionPlugin,
    ...peoplePlugins,
    AccessCredentialFieldsPlugin,
    ...eventConditionPlugins,
    ...transactionConditionPlugins,
    ...paymentConditionPlugins,
    EventDetailsPlugin,
    EventFieldsPlugin,
    ...federationConditionPlugins,
    ...competitionParticipantConditionPlugins,
    ...competitionSuggestionConditionPlugins,
    federationOrderPlugin,
    LegacyShimsPlugin,
    LegacyRelationAliasesPlugin,
    OTELPlugin,
    QueryControlsPlugin,
  ],

  grafast: {
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
    pgJwtSecret: process.env.JWT_SECRET!,
    pgJwtSignOptions: {
      algorithm: 'HS256',
    },
    pgForbidSetofFunctionsToReturnNull: true,
    retryOnInitFail: true,
    sortExport: true,
  },
};

export default preset;
