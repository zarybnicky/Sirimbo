import { NextResponse, type NextRequest } from 'next/server';
import { LogInAsDocument } from '@/graphql/CurrentUser';
import { executeGraphql } from '@/lib/server/graphql';
import { setSessionCookie } from '@/lib/server/session';

// Impersonation ("log in as") is triggered from the Pages-Router admin UI, where
// server actions can't run, so it stays a route handler. executeGraphql forwards
// the admin's own session cookie, which authorizes the logInAs mutation; the
// returned JWT (for the target user) then replaces the session cookie.
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

  let body: { id?: unknown };
  try {
    body = await req.json();
  } catch {
    return NextResponse.json({ error: 'Neplatný požadavek' }, { status: 400 });
  }

  const id = body.id;
  if (typeof id !== 'string' || !id) {
    return NextResponse.json({ error: 'Chybí ID uživatele' }, { status: 400 });
  }

  let data;
  try {
    data = await executeGraphql(LogInAsDocument, { id });
  } catch {
    return NextResponse.json({ error: 'Nepodařilo se přihlásit' }, { status: 403 });
  }

  const result = data.logInAs?.result;
  if (!result?.jwt) {
    return NextResponse.json({ error: 'Nepodařilo se přihlásit' }, { status: 403 });
  }

  await setSessionCookie(result.jwt, host);
  return NextResponse.json({ ok: true });
}
