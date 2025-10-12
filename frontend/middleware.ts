/* eslint-disable import/no-unused-modules */
import { NextResponse } from 'next/server';
import type { NextRequest } from 'next/server';

const TENANT_COOKIE_NAME = 'tenant_id';

export const config = {
  matcher: [
    {
      source: '/((?!_next/static|_next/image|favicon.ico).*)',
      missing: [
        {
          type: 'cookie',
          key: TENANT_COOKIE_NAME,
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

function getHostname(request: NextRequest): string | null {
  const forwardedHost = request.headers.get('x-forwarded-host');
  const hostHeader = forwardedHost ?? request.headers.get('host') ?? request.nextUrl.host;
  return hostHeader?.split(':')[0]?.toLowerCase() ?? null;
}

function resolveTenantId(hostname: string | null): number {
  if (hostname) {
    const tenantId = hostToTenantId.get(hostname);
    if (tenantId) {
      return tenantId;
    }
  }
  return DEFAULT_TENANT_ID;
}

export function middleware(request: NextRequest) {
  const hostname = getHostname(request);
  const tenantId = resolveTenantId(hostname);

  const response = NextResponse.next();

  const stringTenantId = String(tenantId);
  response.cookies.set({
    name: TENANT_COOKIE_NAME,
    value: stringTenantId,
    path: '/',
    sameSite: 'lax',
    secure: request.nextUrl.protocol === 'https:',
  });
  request.cookies.set(TENANT_COOKIE_NAME, stringTenantId);

  return response;
}
