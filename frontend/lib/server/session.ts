import { SESSION_COOKIE, SESSION_PRESENT_COOKIE } from '@/lib/session-cookies';
import { cookies } from 'next/headers';
import type { NextRequest } from 'next/server';

const COOKIE_OPTIONS = {
  secure: process.env.NODE_ENV === 'production',
  sameSite: 'lax' as const,
  path: '/',
  maxAge: 60 * 60 * 24 * 365,
};

export function sameOrigin(request: NextRequest) {
  const origin = request.headers.get('origin');
  const host = (request.headers.get('x-forwarded-host') ?? request.headers.get('host'))
    ?.split(',', 1)[0]
    ?.trim();
  if (!origin || !host) return false;

  try {
    return new URL(origin).host.toLowerCase() === host.toLowerCase();
  } catch {
    return false;
  }
}

export async function setSessionCookie(token: string) {
  const store = await cookies();
  store.set(SESSION_COOKIE, token, { ...COOKIE_OPTIONS, httpOnly: true });
  store.set(SESSION_PRESENT_COOKIE, '1', COOKIE_OPTIONS);
}

export async function clearSessionCookie() {
  const store = await cookies();
  store.set(SESSION_COOKIE, '', { ...COOKIE_OPTIONS, httpOnly: true, maxAge: 0 });
  store.set(SESSION_PRESENT_COOKIE, '', { ...COOKIE_OPTIONS, maxAge: 0 });
}
