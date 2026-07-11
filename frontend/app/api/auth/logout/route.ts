import { NextResponse, type NextRequest } from 'next/server';
import { clearSessionCookie } from '@/lib/server/session';

// Logout is invoked from the Pages-Router app shell (menu button), where server
// actions can't run, so it stays a route handler. The cookie is httpOnly, so
// clearing it must happen server-side.
export async function POST(req: NextRequest) {
  const origin = req.headers.get('origin');
  const host = req.headers.get('host');
  if (origin && host) {
    try {
      if (new URL(origin).host !== host) {
        return NextResponse.json({ error: 'Invalid origin' }, { status: 403 });
      }
    } catch {
      return NextResponse.json({ error: 'Invalid origin' }, { status: 403 });
    }
  }

  await clearSessionCookie(host);
  return NextResponse.json({ ok: true });
}
