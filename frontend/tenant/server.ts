import { defaultTenant, getTenant, hostToTenant } from '@/tenant/catalog';
import { cookies, headers } from 'next/headers';

export async function getRequestTenant() {
  const cookieStore = await cookies();
  const cookieTenant = getTenant(cookieStore.get('tenant_id')?.value);
  if (cookieTenant) return cookieTenant;

  const headerStore = await headers();
  const host = headerStore.get('x-forwarded-host') ?? headerStore.get('host');
  const hostname = host?.split(',')[0]?.trim()?.split(':')[0]?.toLowerCase() || null;

  return hostToTenant.get(hostname ?? '') ?? defaultTenant;
}
