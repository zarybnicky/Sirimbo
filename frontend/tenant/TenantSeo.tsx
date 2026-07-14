'use client';

import { getServerTenant } from '@/tenant/catalog';
import { tenantIdAtom } from '@/ui/state/auth';
import { useAtomValue } from 'jotai';
import { DefaultSeo } from 'next-seo';
import { usePathname } from 'next/navigation';

export function TenantSeo() {
  const tenantId = useAtomValue(tenantIdAtom);
  const tenant = getServerTenant(tenantId);
  const pathname = usePathname() ?? '/';
  const publicSite = tenant.config.publicSite;
  const canonical = new URL(pathname, tenant.config.origin).toString();
  const publicImage = publicSite?.image
    ? {
        ...publicSite.image,
        url: new URL(publicSite.image.url, tenant.config.origin).toString(),
      }
    : undefined;
  return (
    <DefaultSeo
      {...tenant.config.seo}
      canonical={canonical}
      openGraph={{
        ...tenant.config.seo.openGraph,
        url: canonical,
        images: publicImage ? [publicImage] : tenant.config.seo.openGraph.images,
      }}
    />
  );
}
