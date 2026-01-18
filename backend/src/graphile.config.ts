import express from 'express';
import 'graphile-config';
import jwt from 'jsonwebtoken';
import path from 'path';
import { PostGraphileAmberPreset } from 'postgraphile/presets/amber';
import { makeV4Preset } from 'postgraphile/presets/v4';
import { pool } from './db.ts';
import { PgSimplifyInflectionPreset } from '@graphile/simplify-inflection';
import 'postgraphile/grafserv/express/v4';
import { LRUCache } from 'lru-cache';
import filePlugin from './plugins/file.ts';
import currentUserPlugin from './plugins/current-user.ts';
import { makePgService } from 'postgraphile/@dataplan/pg/adaptors/pg';

const isDevelopment = process.env.NODE_ENV === 'development';

const JWT_SECRET = process.env.JWT_SECRET!;
if (!JWT_SECRET && process.env.NODE_ENV !== 'development') {
  throw new Error('JWT_SECRET must be set');
}

const tenantCache = new LRUCache<string, string>({
  max: 500,
  ttl: 5 * 60 * 1000, // 5 min
});

function normalizeOrigin(req: express.Request) {
  const raw = req.get('origin');
  if (!raw) return '';
  try {
    return new URL(raw).origin.toLowerCase();
  } catch {
    return '';
  }
}

async function findTenantId(req: express.Request): Promise<string> {
  const tenantId = req.get('x-tenant-id');
  if (tenantId) return tenantId;

  const hostname = (req.hostname || '').toLowerCase();
  const origin = normalizeOrigin(req);
  const key = `${hostname}|${origin}`;

  const cached = tenantCache.get(key);
  if (cached) return cached;

  const {
    rows: [host],
  } = await pool.query(
    'select id from tenant where $1 = any (origins) or $2 = any (origins)',
    [hostname, origin],
  );
  const resolved = host ? host.id : '1';
  tenantCache.set(key, resolved);
  return resolved;
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
  if (!token && req.cookies.rozpisovnik && req.cookies.rozpisovnik.length > 9) {
    token = req.cookies.rozpisovnik;
  }
  if (!token) return settings;

  const claims = jwt.verify(token, JWT_SECRET, {
    ignoreExpiration: true,
    algorithms: ['HS256'],
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
  plugins: [...filePlugin, currentUserPlugin],

  grafast: {
    async context(ctx) {
      const { req } = ctx.expressv4 ?? {};
      return {
        pgSettings: req ? await loadUserFromSession(req) : {},
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
