/* eslint-disable import-x/no-unused-modules */
import { setSessionCookie, sameOrigin } from '@/lib/server/session';
import { NextResponse, type NextRequest } from 'next/server';

export async function POST(request: NextRequest) {
  if (!sameOrigin(request)) {
    return NextResponse.json({ error: 'Invalid origin' }, { status: 403 });
  }

  const { token } = await request.json().catch(() => ({ token: null }));
  if (typeof token !== 'string' || token.split('.').length !== 3) {
    return NextResponse.json({ error: 'Neplatný token' }, { status: 400 });
  }

  await setSessionCookie(token);
  return NextResponse.json({ ok: true });
}
