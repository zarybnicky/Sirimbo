import { NextResponse, type NextRequest } from 'next/server';
import { cookies } from 'next/headers';
import { CurrentUserDocument } from '@/graphql/CurrentUser';
import { executeGraphql } from '@/lib/server/graphql';
import { SESSION_COOKIE, setSessionCookie } from '@/lib/server/session';
import { decodeClaims } from '@/lib/session-claims';

// GET: "who am I" — returns the current user (resolved from the session cookie)
// and the session claims (roles / per-tenant arrays), both as plain data so the
// frontend never decodes a JWT itself.
export async function GET() {
  const token = (await cookies()).get(SESSION_COOKIE)?.value;
  const claims = decodeClaims(token);

  let user = null;
  try {
    const data = await executeGraphql(CurrentUserDocument);
    user = data.getCurrentUser ?? null;
  } catch {
    user = null;
  }

  return NextResponse.json({ user, claims });
}

// POST: establish the httpOnly session cookie from a token the client already
// holds (the legacy localStorage bearer) and return its claims, so UserRefresher
// can upgrade a pre-existing session and seed state in one step. Planting a token
// grants no new privilege (it's the same bearer credential the backend verifies),
// so no round-trip validation is needed here.
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
  return NextResponse.json({ ok: true, claims: decodeClaims(token) });
}
