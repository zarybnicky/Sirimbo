import 'server-only';

import { CurrentUserDocument } from '@/graphql/CurrentUser';
import type { RequestAuthState } from '@/lib/auth';
import { normalizeClaims } from '@/lib/auth-claims';
import { buildId } from '@/lib/build-id';
import { executeGraphql } from '@/lib/server/graphql';
import { SESSION_COOKIE } from '@/lib/session-cookies';
import { cookies } from 'next/headers';
import type { NextRequest, NextResponse } from 'next/server';
import { cache } from 'react';

const COOKIE_OPTIONS = {
  secure: process.env.NODE_ENV === 'production',
  sameSite: 'lax' as const,
  path: '/',
  maxAge: 60 * 60 * 24 * 365,
};

export const getRequestAuth = cache(async (): Promise<RequestAuthState> => {
  const cookieStore = await cookies();
  if (!cookieStore.has(SESSION_COOKIE)) return { claims: null, user: null };

  const data = await executeGraphql(CurrentUserDocument, { versionId: buildId });
  return { claims: normalizeClaims(data.currentClaims), user: data.getCurrentUser };
});

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

export async function setSessionCookie(token: string, response?: NextResponse) {
  const store = response?.cookies ?? await cookies();
  store.set(SESSION_COOKIE, token, { ...COOKIE_OPTIONS, httpOnly: true });
}

export async function clearSessionCookie() {
  const store = await cookies();
  store.set(SESSION_COOKIE, '', { ...COOKIE_OPTIONS, httpOnly: true, maxAge: 0 });
}

export function clearTenantCookie(request: NextRequest, response: NextResponse) {
  const host = (request.headers.get('x-forwarded-host') ?? request.headers.get('host') ?? request.nextUrl.host)
    .split(',', 1)[0]!.trim().split(':', 1)[0]!.toLowerCase();
  const cookie = 'tenant_id=; Path=/; Max-Age=0; SameSite=Lax';
  response.headers.append('Set-Cookie', cookie);
  for (const domain of new Set([host, host.replace(/^www\./, '')])) {
    response.headers.append('Set-Cookie', `${cookie}; Domain=${domain}`);
  }
}
