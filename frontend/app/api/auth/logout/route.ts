/* eslint-disable import-x/no-unused-modules */
import { clearSessionCookie, sameOrigin } from '@/lib/server/session';
import { NextResponse, type NextRequest } from 'next/server';

export async function POST(request: NextRequest) {
  if (!sameOrigin(request)) {
    return NextResponse.json({ error: 'Invalid origin' }, { status: 403 });
  }

  await clearSessionCookie();
  return NextResponse.json({ ok: true });
}
