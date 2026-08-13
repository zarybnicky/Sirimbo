/* eslint-disable import-x/no-unused-modules */
import { LogInAsDocument } from '@/graphql/CurrentUser';
import { executeGraphql } from '@/lib/server/graphql';
import { setSessionCookie, sameOrigin } from '@/lib/server/session';
import { NextResponse, type NextRequest } from 'next/server';

export async function POST(request: NextRequest) {
  if (!sameOrigin(request)) {
    return NextResponse.json({ error: 'Invalid origin' }, { status: 403 });
  }

  const { id } = await request.json().catch(() => ({ id: null }));
  if (typeof id !== 'string' || !id) {
    return NextResponse.json({ error: 'Chybí ID uživatele' }, { status: 400 });
  }

  try {
    const { logInAs } = await executeGraphql(LogInAsDocument, { id });
    const token = logInAs?.result?.jwt;
    if (!token) {
      return NextResponse.json({ error: 'Přihlášení selhalo' }, { status: 403 });
    }

    await setSessionCookie(token);
    return NextResponse.json({ ok: true });
  } catch {
    return NextResponse.json({ error: 'Přihlášení selhalo' }, { status: 403 });
  }
}
