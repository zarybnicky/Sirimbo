/* eslint-disable import-x/no-unused-modules */
import { setSessionCookie, sameOrigin } from '@/lib/server/session';
import { NextResponse, type NextRequest } from 'next/server';
import { withRequestPgClient } from '@/lib/server/postgresql';
import { SESSION_COOKIE } from '@/lib/session-cookies';
import { cookies, headers } from 'next/headers';

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

type DatabaseSession = {
  current_role: string;
  tenant_id: string | null;
  current_tenant_id: string;
  can_insert_file: boolean;
  can_use_file_sequence: boolean;
};

export async function GET() {
  const [cookieStore, headerStore] = await Promise.all([cookies(), headers()]);
  return await withRequestPgClient(async (client, settings) => {
    const result = await client.query<DatabaseSession>(
      `select
         current_role,
         current_setting('jwt.claims.tenant_id', true) as tenant_id,
         current_tenant_id()::text as current_tenant_id,
         has_table_privilege(current_user, 'public.file', 'INSERT') as can_insert_file,
         has_sequence_privilege(current_user, 'public.file_id_seq', 'USAGE') as can_use_file_sequence`,
    );

    return {
      sessionCookiePresent: cookieStore.has(SESSION_COOKIE),
      tenantCookie: cookieStore.get('tenant_id')?.value ?? null,
      host: headerStore.get('x-forwarded-host') ?? headerStore.get('host'),
      request: settings,
      database: result.rows[0]!,
    };
  });
}
