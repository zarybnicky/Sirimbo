/* eslint-disable import/no-unused-modules */
import { NextResponse } from 'next/server';
import type { NextRequest } from 'next/server';
import { serverTenantCatalog } from './tenant/catalog-server';

export const config = {
  matcher: [
    {
      source: '/((?!_next/static|_next/image|favicon.ico).*)',
      missing: [{ type: 'cookie', key: 'tenant_id' }],
    },
  ],
};

const hostToTenantId = new Map<string, number>();

for (const entry of Object.values(serverTenantCatalog)) {
  for (const host of entry.hosts) {
    hostToTenantId.set(host.toLowerCase(), entry.id);
  }
}

export function middleware(request: NextRequest) {
  const forwardedHost = request.headers.get('x-forwarded-host');
  const hostHeader = forwardedHost ?? request.headers.get('host') ?? request.nextUrl.host;
  const hostname = hostHeader?.split(':')[0]?.toLowerCase() ?? null;
  const tenantId = hostToTenantId.get(hostname ?? '') ?? '1';

  const response = NextResponse.next({
    headers: {
      cookie: `tenant_id=${tenantId}`,
    },
  });

  response.cookies.set({
    name: 'tenant_id',
    value: String(tenantId),
    path: '/',
    sameSite: 'lax',
    secure: request.nextUrl.protocol === 'https:',
    domain: hostname ?? request.nextUrl.hostname ?? undefined,
    expires: Date.now() + 1000 * 60 * 60 * 24 * 365 * 10,
  });

  return response;
}
