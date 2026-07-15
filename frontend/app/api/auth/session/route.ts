import { NextResponse, type NextRequest } from 'next/server';
import { cookies } from 'next/headers';
import { CurrentUserDocument } from '@/graphql/CurrentUser';
import { executeGraphql } from '@/lib/server/graphql';
import { SESSION_COOKIE, sameOrigin, setSessionCookie } from '@/lib/server/session';
import { decodeClaims } from '@/lib/session-claims';

// "Who am I": current user + session claims, resolved from the cookie.
export async function GET() {
  const claims = decodeClaims((await cookies()).get(SESSION_COOKIE)?.value);
  try {
    const { getCurrentUser } = await executeGraphql(CurrentUserDocument);
    return NextResponse.json({ user: getCurrentUser ?? null, claims });
  } catch {
    return NextResponse.json({ user: null, claims });
  }
}

// Establish the cookie from a token the client already holds (legacy upgrade).
export async function POST(req: NextRequest) {
  if (!sameOrigin(req)) return NextResponse.json({ error: 'Invalid origin' }, { status: 403 });

  const { token } = await req.json().catch(() => ({ token: null }));
  if (typeof token !== 'string' || token.split('.').length !== 3) {
    return NextResponse.json({ error: 'Neplatný token' }, { status: 400 });
  }

  await setSessionCookie(token, req.headers.get('host'));
  return NextResponse.json({ ok: true, claims: decodeClaims(token) });
}
