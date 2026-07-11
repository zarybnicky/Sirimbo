import { cookies } from 'next/headers';

// The session JWT lives in this cookie. We intentionally reuse the existing
// `rozpisovnik` name so the backend token fallback (backend/src/auth.ts) and the
// server-side forwarding in lib/server/graphql.ts keep working unchanged. The
// difference from the legacy client-written cookie is that this one is set
// server-side, httpOnly, and scoped to the whole site (path=/) instead of /f.
export const SESSION_COOKIE = 'rozpisovnik';

// The backend verifies with ignoreExpiration, so this is only a client-retention
// hint. Match the previous client behaviour of a long-lived cookie.
const MAX_AGE = 60 * 60 * 24 * 365;

// Mirrors the domain logic the client used previously (auth.ts): scope to the
// current host (minus www) so tenant subdomains share the session, but leave it
// host-only on localhost.
export function cookieDomainForHost(hostname: string | null | undefined): string | undefined {
  if (!hostname) return undefined;
  const host = hostname.split(':')[0]?.toLowerCase() ?? '';
  if (host === 'localhost' || host === '127.0.0.1') return undefined;
  return host.replace(/^www\./, '');
}

export async function setSessionCookie(token: string, hostname?: string | null) {
  const cookieStore = await cookies();
  cookieStore.set(SESSION_COOKIE, token, {
    httpOnly: true,
    secure: process.env.NODE_ENV === 'production',
    sameSite: 'lax',
    path: '/',
    maxAge: MAX_AGE,
    domain: cookieDomainForHost(hostname),
  });
}

export async function clearSessionCookie(hostname?: string | null) {
  const cookieStore = await cookies();
  cookieStore.set(SESSION_COOKIE, '', {
    httpOnly: true,
    secure: process.env.NODE_ENV === 'production',
    sameSite: 'lax',
    path: '/',
    maxAge: 0,
    domain: cookieDomainForHost(hostname),
  });
}
