import { defaultTenant, hostToTenant, parseTenant } from '@/tenant/catalog';
import { cookies, headers } from 'next/headers';

export async function getRequestTenant() {
  const headerStore = await headers();
  const cookieStore = await cookies();

  const host = headerStore.get('x-forwarded-host') ?? headerStore.get('host');
  const hostname = host?.split(',')[0]?.trim()?.split(':')[0]?.toLowerCase() || null;
  const cookieTenant = parseTenant(cookieStore.get('tenant_id')?.value);

  return cookieTenant ?? hostToTenant.get(hostname ?? '') ?? defaultTenant;
}
