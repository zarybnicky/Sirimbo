import type { NextRequest } from 'next/server';
import { NextResponse } from 'next/server';
import { defaultTenant, hostToTenant, parseTenant } from './tenant/catalog';

// eslint-disable-next-line import-x/no-unused-modules
export const config = {
  matcher: [
    {
      source: '/((?!_next/static|_next/image|favicon.ico).*)',
    },
  ],
};

// eslint-disable-next-line import-x/no-unused-modules
export function proxy(request: NextRequest) {
  const forwardedHost = request.headers.get('x-forwarded-host');
  const hostHeader = forwardedHost ?? request.headers.get('host') ?? request.nextUrl.host;
  const hostname = hostHeader?.split(',')[0]?.trim().split(':')[0]?.toLowerCase() ?? null;
  const previousTenantId = request.cookies.get('tenant_id')?.value;
  const tenant =
    parseTenant(previousTenantId) ?? hostToTenant.get(hostname ?? '') ?? defaultTenant;
  const tenantId = String(tenant.id);

  request.cookies.set('tenant_id', tenantId);

  const memberHomePath =
    request.nextUrl.pathname === '/' ||
    request.nextUrl.pathname === '/clanky' ||
    request.nextUrl.pathname.startsWith('/clanky/');

  let response: NextResponse;
  if (!tenant.config.publicSite && memberHomePath) {
    const destination = request.nextUrl.clone();
    destination.pathname = '/dashboard';
    response = NextResponse.rewrite(destination);
  } else {
    response = NextResponse.next();
  }

  if (previousTenantId !== tenantId) {
    response.cookies.set({
      name: 'tenant_id',
      value: tenantId,
      path: '/',
      sameSite: 'lax',
      secure: request.nextUrl.protocol === 'https:',
      domain: hostname ?? request.nextUrl.hostname ?? undefined,
      expires: Date.now() + 1000 * 60 * 60 * 24 * 365 * 10,
    });
  }

  return response;
}
