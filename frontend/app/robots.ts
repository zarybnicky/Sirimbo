/* eslint-disable import-x/no-unused-modules */
import { getRequestTenant } from '@/lib/tenant/server';
import type { MetadataRoute } from 'next';

export const dynamic = 'force-dynamic';

export default async function robots(): Promise<MetadataRoute.Robots> {
  const tenant = await getRequestTenant();

  if (!tenant.config.publicSite) {
    return {
      rules: {
        crawlDelay: 1,
      },
    };
  }

  const origin = tenant.config.publicSite?.origin ?? `https://${tenant.hosts[0]}`;
  return {
    rules: {
      crawlDelay: 1,
    },
    sitemap: new URL('/sitemap.xml', origin).toString(),
  };
}
