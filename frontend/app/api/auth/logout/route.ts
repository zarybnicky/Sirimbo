import { NextResponse, type NextRequest } from 'next/server';
import { clearSessionCookie, sameOrigin } from '@/lib/server/session';

export async function POST(req: NextRequest) {
  if (!sameOrigin(req)) return NextResponse.json({ error: 'Invalid origin' }, { status: 403 });
  await clearSessionCookie(req.headers.get('host'));
  return NextResponse.json({ ok: true });
}
