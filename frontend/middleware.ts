/* eslint-disable import/no-unused-modules */
import { NextResponse } from 'next/server';
import type { NextRequest } from 'next/server';

export const config = {
  matcher: [
    {
      source: '/((?!_next/static|_next/image|favicon.ico).*)',
      missing: [
        {
          type: 'cookie',
          key: 'tenant_id',
        },
      ],
    },
  ],
};

const TENANT_HOST_ENTRIES: Array<{ id: number; hosts: string[] }> = [
  { id: 1, hosts: ['olymp.rozpisovnik.cz', 'tkolymp.cz'] },
  { id: 2, hosts: ['dspkometa.rozpisovnik.cz'] },
  { id: 3, hosts: ['tkstarlet.rozpisovnik.cz'] },
  { id: 4, hosts: ['dspkometa2.rozpisovnik.cz'] },
];

const hostToTenantId = new Map<string, number>();

for (const entry of TENANT_HOST_ENTRIES) {
  for (const host of entry.hosts) {
    hostToTenantId.set(host.toLowerCase(), entry.id);
  }
}

const DEFAULT_TENANT_ID = Number.parseInt(process.env.NEXT_PUBLIC_TENANT_ID ?? '1', 10);

export function middleware(request: NextRequest) {
  const forwardedHost = request.headers.get('x-forwarded-host');
  const hostHeader = forwardedHost ?? request.headers.get('host') ?? request.nextUrl.host;
  const hostname = hostHeader?.split(':')[0]?.toLowerCase() ?? null;
  const tenantId = hostToTenantId.get(hostname ?? '') ?? DEFAULT_TENANT_ID;

  const response = NextResponse.next();

  response.cookies.set({
    name: 'tenant_id',
    value: String(tenantId),
    path: '/',
    sameSite: 'lax',
    secure: request.nextUrl.protocol === 'https:',
    domain: hostname ?? request.nextUrl.hostname ?? undefined,
  });

  return response;
}
