import express from 'express';
import 'graphile-config';
import jwt from 'jsonwebtoken';
import { pool } from './db.ts';
import 'postgraphile/grafserv/express/v4';
import { LRUCache } from 'lru-cache';
import {
  makePgAdaptorWithPgClient,
  type NodePostgresPgClient,
} from '@dataplan/pg/adaptors/pg';

export const JWT_SECRET = process.env.JWT_SECRET!;
if (!JWT_SECRET && process.env.NODE_ENV !== 'development') {
  throw new Error('JWT_SECRET must be set');
}

declare global {
  namespace Express {
    interface Request {
      pgSettings: Record<string, string>;
    }
  }
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

function getBearerOrCookie(req: express.Request): string | undefined {
  const authorization = req.get('authorization');
  if (authorization?.toLowerCase().startsWith('bearer ')) {
    return authorization.substring(7);
  }
  if (req.cookies.rozpisovnik && req.cookies.rozpisovnik.length > 9) {
    return req.cookies.rozpisovnik;
  }
  return undefined;
}

function verifySessionToken(token: string): jwt.JwtPayload | undefined {
  try {
    return jwt.verify(token, JWT_SECRET, {
      ignoreExpiration: true,
      algorithms: ['HS256'],
    }) as jwt.JwtPayload;
  } catch (error) {
    // Stale or tampered tokens should downgrade to anonymous access.
    if (error instanceof jwt.JsonWebTokenError) {
      return undefined;
    }
    throw error;
  }
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

  const token = getBearerOrCookie(req);
  if (!token) return settings;

  const claims = verifySessionToken(token);
  if (!claims) return settings;

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

export function authContext() {
  return async (req: express.Request, _res: unknown, next: express.NextFunction) => {
    try {
      req.pgSettings = await loadUserFromSession(req);
      next();
    } catch (error) {
      next(error);
    }
  };
}

export function withPgClientAndPgSettings<T>(
  req: express.Request,
  fn: (client: NodePostgresPgClient) => T,
): Promise<T> {
  const withPgClient = makePgAdaptorWithPgClient(pool);
  return withPgClient(req.pgSettings, fn);
}
