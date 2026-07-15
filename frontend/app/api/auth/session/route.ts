import { NextResponse, type NextRequest } from 'next/server';
import { CurrentUserDocument } from '@/graphql/CurrentUser';
import { executeGraphql } from '@/lib/server/graphql';
import { setSessionCookie } from '@/lib/server/session';

// GET: "who am I" — resolves the current user from the session cookie (which
// executeGraphql forwards). Returns null when unauthenticated.
export async function GET() {
  try {
    const data = await executeGraphql(CurrentUserDocument);
    return NextResponse.json({ user: data.getCurrentUser ?? null });
  } catch {
    return NextResponse.json({ user: null });
  }
}

// POST: establish the httpOnly session cookie from a token the client already
// holds (the legacy localStorage bearer). This lets UserRefresher upgrade
// pre-existing sessions to the cookie with zero friction — no re-login. Planting
// a token grants no new privilege (it's the same bearer credential the backend
// already verifies), so no server round-trip is needed to validate it here.
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

  let body: { token?: unknown };
  try {
    body = await req.json();
  } catch {
    return NextResponse.json({ error: 'Neplatný požadavek' }, { status: 400 });
  }

  const token = body.token;
  if (typeof token !== 'string' || token.split('.').length !== 3) {
    return NextResponse.json({ error: 'Neplatný token' }, { status: 400 });
  }

  await setSessionCookie(token, host);
  return NextResponse.json({ ok: true });
}
