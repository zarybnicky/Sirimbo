import { cookies } from 'next/headers';
import type { NextRequest } from 'next/server';

// Reuse the existing `rozpisovnik` cookie (backend fallback + server forwarding
// already read it); the difference is it's now set server-side, httpOnly, and
// site-wide (path=/) rather than client-side at /f.
export const SESSION_COOKIE = 'rozpisovnik';

const MAX_AGE = 60 * 60 * 24 * 365;

export function cookieDomainForHost(hostname: string | null | undefined): string | undefined {
  const host = hostname?.split(':')[0]?.toLowerCase() ?? '';
  if (!host || host === 'localhost' || host === '127.0.0.1') return undefined;
  return host.replace(/^www\./, '');
}

function cookieOptions(hostname?: string | null, maxAge = MAX_AGE) {
  return {
    httpOnly: true,
    secure: process.env.NODE_ENV === 'production',
    sameSite: 'lax' as const,
    path: '/',
    maxAge,
    domain: cookieDomainForHost(hostname),
  };
}

export async function setSessionCookie(token: string, hostname?: string | null) {
  (await cookies()).set(SESSION_COOKIE, token, cookieOptions(hostname));
}

export async function clearSessionCookie(hostname?: string | null) {
  (await cookies()).set(SESSION_COOKIE, '', cookieOptions(hostname, 0));
}

// Cross-origin guard for state-changing routes (the cookie is auto-attached).
export function sameOrigin(req: NextRequest): boolean {
  const origin = req.headers.get('origin');
  const host = req.headers.get('host');
  if (!origin || !host) return true;
  try {
    return new URL(origin).host === host;
  } catch {
    return false;
  }
}
