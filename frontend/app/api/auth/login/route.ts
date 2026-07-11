import { NextResponse, type NextRequest } from 'next/server';
import { LoginDocument } from '@/graphql/CurrentUser';
import { executeGraphql } from '@/lib/server/graphql';
import { setSessionCookie } from '@/lib/server/session';

// Auth mutations run server-to-server here so the resulting JWT can be planted
// in an httpOnly cookie the client never sees. Other API consumers keep using
// the bearer token against /graphql directly.
export async function POST(req: NextRequest) {
  // Basic CSRF guard: the cookie is now auto-attached, so reject cross-origin posts.
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

  let body: { login?: unknown; passwd?: unknown };
  try {
    body = await req.json();
  } catch {
    return NextResponse.json({ error: 'Neplatný požadavek' }, { status: 400 });
  }

  const { login, passwd } = body;
  if (typeof login !== 'string' || typeof passwd !== 'string' || !login || !passwd) {
    return NextResponse.json({ error: 'Zadejte jméno a heslo' }, { status: 400 });
  }

  let data;
  try {
    data = await executeGraphql(LoginDocument, { login, passwd });
  } catch {
    return NextResponse.json(
      { error: 'Nesprávné jméno nebo heslo' },
      { status: 401 },
    );
  }

  const result = data.login?.result;
  if (!result?.jwt) {
    return NextResponse.json(
      { error: 'Nesprávné jméno nebo heslo' },
      { status: 401 },
    );
  }

  await setSessionCookie(result.jwt, host);
  return NextResponse.json({ user: result.usr ?? null });
}
