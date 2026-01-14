import express from 'express';
import 'graphile-config';
import jwt from 'jsonwebtoken';
import path from 'path';
import * as adaptor from 'postgraphile/@dataplan/pg/adaptors/pg';
import { PostGraphileAmberPreset } from 'postgraphile/presets/amber';
import { makeV4Preset } from 'postgraphile/presets/v4';
import { pool, poolGraphqlContext } from './db.ts';
import { PgSimplifyInflectionPreset } from '@graphile/simplify-inflection';
import 'postgraphile/grafserv/express/v4';

import proxyPlugin from './plugins/proxy.ts';
import filePlugin from './plugins/file.ts';
import currentUserPlugin from './plugins/current-user.ts';

const isDevelopment = process.env.NODE_ENV === 'development';

async function findTenantId(req: express.Request): Promise<string> {
  const tenantId = req.get('x-tenant-id');
  if (tenantId) return tenantId;

  const {
    rows: [host],
  } = await pool.query(
    'select id from tenant where $1 = any (origins) or $2 = any (origins)',
    [req.headers.host, req.headers.origin],
  );
  if (host) {
    return host.id;
  }
  return '1';
}

async function loadUserFromSession(req: express.Request): Promise<{ [k: string]: any }> {
  const tenantId = await findTenantId(req);
  const settings: Record<string, string> = {
    role: 'anonymous',
    'jwt.claims.tenant_id': tenantId,
    'jwt.claims.user_id': '',
    'jwt.claims.username': '',
    'jwt.claims.email': '',
    'jwt.claims.my_person_ids': '[]',
    'jwt.claims.my_tenant_ids': '[]',
    'jwt.claims.my_cohort_ids': '[]',
    'jwt.claims.my_couple_ids': '[]',
  };

  let token;

  const authorization = req.get('authorization');
  if (authorization?.toLowerCase().startsWith('bearer ')) {
    token = authorization.substring(7);
  }
  if (!token && req.cookies.auth) {
    token = req.cookies.auth;
  }
  if (!token) return settings;

  const claims = jwt.verify(token, process.env.JWT_SECRET || '', {
    ignoreExpiration: true,
  }) as jwt.JwtPayload;

  settings.role = claims.is_system_admin
    ? 'system_admin'
    : claims.is_admin
      ? 'administrator'
      : claims.is_trainer
        ? 'trainer'
        : claims.is_member
          ? 'member'
          : 'anonymous';

  for (const key in claims) {
    if (['exp', 'aud', 'iat', 'iss'].includes(key)) continue;
    if (Array.isArray(claims[key])) {
      settings[`jwt.claims.${key}`] = '{' + claims[key].map(String).join(',') + '}';
    } else {
      settings[`jwt.claims.${key}`] = claims[key];
    }
  }

  // FIXME: Or verify claims.tenant === request.tenant, otherwise log out?
  settings['jwt.claims.tenant_id'] = tenantId;

  return settings;
}

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
  plugins: [...proxyPlugin, ...filePlugin, currentUserPlugin],

  grafast: {
    async context(ctx) {
      const { req } = ctx.expressv4 ?? {};
      return {
        pgSettings: req ? await loadUserFromSession(req) : {},
        ...poolGraphqlContext,
      };
    },
    explain: isDevelopment,
  },
  grafserv: {
    port: Number.parseInt(process.env.PORT || '5000', 10),
    watch: !process.env.POSTGRAPHILE_DONT_WATCH,
    graphiql: isDevelopment,
  },
  gather: {
    pgJwtTypes: 'public.jwt_token',
  },
  schema: {
    pgJwtSecret: process.env.JWT_SECRET || '',
    pgForbidSetofFunctionsToReturnNull: true,
    retryOnInitFail: true,
    sortExport: true,
    exportSchemaSDLPath: isDevelopment ? path.resolve('../schema.graphql') : undefined,
  },

  pgServices: [
    {
      name: 'main',
      schemas: ['public'],
      pgSettingsKey: 'pgSettings',
      pgSubscriberKey: 'pgSubscriber' as any as undefined,
      withPgClientKey: 'withPgClient',
      adaptor,
      adaptorSettings: {
        pool,
        // superuserConnectionString: process.env.SUPERUSER_DATABASE_URL,
      },
      pgSubscriber: new adaptor.PgSubscriber(pool),
    },
  ],
};

export default preset;
