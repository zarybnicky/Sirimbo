import type { NextRequest } from 'next/server';
import { NextResponse } from 'next/server';
import { defaultTenant, hostToTenant } from './tenant/catalog';

export const config = {
  matcher: [
    {
      source: '/((?!_next/static|_next/image|favicon.ico).*)',
      missing: [{ type: 'cookie', key: 'tenant_id' }],
    },
  ],
};

export function proxy(request: NextRequest) {
  const forwardedHost = request.headers.get('x-forwarded-host');
  const hostHeader = forwardedHost ?? request.headers.get('host') ?? request.nextUrl.host;
  const hostname = hostHeader?.split(',', 1)[0]?.trim().split(':', 1)[0]?.toLowerCase() ?? null;
  const tenant = hostToTenant.get(hostname ?? '') ?? defaultTenant;
  const tenantId = String(tenant.id);

  request.cookies.set('tenant_id', tenantId);

  const response = NextResponse.next();
  response.cookies.set({
    name: 'tenant_id',
    value: tenantId,
    path: '/',
    sameSite: 'lax',
    secure: request.nextUrl.protocol === 'https:',
    domain: hostname ?? request.nextUrl.hostname ?? undefined,
    expires: Date.now() + 1000 * 60 * 60 * 24 * 365 * 10,
  });

  return response;
}
