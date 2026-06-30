import { getServerTenant } from '@/tenant/catalog-server';
import { DefaultSeo } from 'next-seo';

export function TenantSeo({ tenantId }: { tenantId: string }) {
  const entry = getServerTenant(tenantId);
  return <DefaultSeo {...entry.config.seo} />;
}
