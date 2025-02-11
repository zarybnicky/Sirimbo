import * as adaptor from "postgraphile/@dataplan/pg/adaptors/pg";
import express from 'express';
import "graphile-config";
import { JwtPayload, verify as verifyJwt } from 'jsonwebtoken';
import path from 'path';
import { PostGraphileAmberPreset } from "postgraphile/presets/amber";
import { makeV4Preset } from "postgraphile/presets/v4";
import { pool } from './db.js';

const isDevelopment = process.env.NODE_ENV === 'development';

async function findTenantId(req: express.Request): Promise<string> {
  if (req.headersDistinct['x-tenant-id']?.length) {
    return req.headersDistinct['x-tenant-id'][0];
  }

  const { rows: [host] } = await pool.query(
    'select id from tenant where $1 = any (origins) or $2 = any (origins)',
    [req.headers.host, req.headers.origin],
  );

  return host ? host.id : '1';
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
    'internal.wdsf_auth': process.env.WDSF_AUTH || '',
  };

  const authorization = req.get('authorization');
  if (authorization?.toLowerCase().startsWith('bearer ')) {
    const token = authorization.substring(7)
    const claims = verifyJwt(token, process.env.JWT_SECRET || '', {
      ignoreExpiration: true
    }) as JwtPayload;
    settings.role = claims.is_admin ? 'administrator' : claims.is_trainer ? 'trainer' : claims.is_member ? 'member' : 'anonymous';

    for (const key in claims) {
      if (['exp', 'aud', 'iat', 'iss'].includes(key)) continue
      if (Array.isArray(claims[key])) {
        settings[`jwt.claims.${key}`] = '[' + claims[key].map((x: string) => `${x}`).join(',') + ']';
      } else {
        settings[`jwt.claims.${key}`] = claims[key];
      }
    }

    // FIXME: Or verify claims.tenant === request.tenant, otherwise log out?
    settings['jwt.claims.tenant_id'] = tenantId;
  }

  return settings;
}


const preset: GraphileConfig.Preset = {
  extends: [
    PostGraphileAmberPreset,

    makeV4Preset({
      retryOnInitFail: true,
      dynamicJson: true,
      setofFunctionsContainNulls: false,
      ignoreRBAC: false,
      extendedErrors: ['hint', 'detail', 'errcode', 'where'],
      showErrorStack: 'json',
      watchPg: true,
      sortExport: true,

      jwtPgTypeIdentifier: 'public.jwt_token',

      // graphiql: isDevelopment,
      // enhanceGraphiql: isDevelopment,
      graphiql: false,
      allowExplain: isDevelopment,
      exportGqlSchemaPath: isDevelopment ? path.resolve('../schema.graphql') : undefined,
    }),
  ],

  disablePlugins: ["NodePlugin"],
  plugins: [
    '@graphile-contrib/pg-simplify-inflector',
    ...require('./plugins/file').default,
    ...require('./plugins/proxy').default,
  ],

  grafast: {
    async context(ctx) {
      const { req } = ctx.expressv4 ?? {};
      return {
        pgSettings: req ? await loadUserFromSession(req) : {},
      };
    },
  },
  grafserv: {
    port: Number.parseInt(process.env.PORT || '5000', 10),
  },

  pgServices: [
    {
      name: "main",
      schemas: ["app_public"],
      pgSettingsKey: "pgSettings",
      withPgClientKey: "withPgClient",
      adaptor,
      adaptorSettings: {
        pool,
        // superuserConnectionString: process.env.SUPERUSER_DATABASE_URL,
      },
    },
  ],
};

export default preset;
