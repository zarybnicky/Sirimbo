import { defaultTenant, hostToTenant, parseTenant } from '@/tenant/catalog';
import { cookies, headers } from 'next/headers';

export async function getRequestTenant() {
  const cookieStore = await cookies();

  const headerStore = await headers();
  const host = headerStore.get('x-forwarded-host') ?? headerStore.get('host');
  const hostname = host?.split(',')[0]?.trim()?.split(':')[0]?.toLowerCase() || null;

  return (
    parseTenant(cookieStore.get('tenant_id')?.value) ??
    hostToTenant.get(hostname ?? '') ??
    defaultTenant
  );
}
