import { getServerTenant, hostToTenantId } from '@/tenant/catalog-server';
import { cookies, headers } from 'next/headers';

function hostnameFromHeader(value: string | null): string | null {
  const host = value?.split(',')[0]?.trim();
  return host?.split(':')[0]?.toLowerCase() || null;
}

function isLocalHostname(hostname: string | null) {
  return hostname === 'localhost' || hostname === '127.0.0.1';
}

export async function getRequestTenant() {
  const headerStore = await headers();
  const cookieStore = await cookies();

  const forwardedHost = headerStore.get('x-forwarded-host');
  const host = forwardedHost ?? headerStore.get('host');
  const hostname = hostnameFromHeader(host);
  const hostTenantId = hostToTenantId.get(hostname ?? '');
  const cookieTenantId = cookieStore.get('tenant_id')?.value;

  return getServerTenant(hostTenantId ?? cookieTenantId ?? '2');
}
