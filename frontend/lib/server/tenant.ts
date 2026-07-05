import {
  hostToTenant,
  parseTenant,
  serverTenantCatalog,
} from '@/tenant/catalog-server';
import { cookies, headers } from 'next/headers';

export async function getRequestTenant() {
  const headerStore = await headers();
  const cookieStore = await cookies();

  const host = headerStore.get('x-forwarded-host') ?? headerStore.get('host');
  const hostname = host?.split(',')[0]?.trim()?.split(':')[0]?.toLowerCase() || null;
  const cookieTenant = parseTenant(cookieStore.get('tenant_id')?.value);
  const hostTenant = hostToTenant.get(hostname ?? '');

  return cookieTenant ?? hostTenant ?? serverTenantCatalog[2];
}
