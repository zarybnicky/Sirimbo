'use client';

import { getServerTenant } from '@/tenant/catalog-server';
import { DefaultSeo } from 'next-seo';
import { usePathname } from 'next/navigation';

export function TenantSeo({
  tenantId,
  noindex,
}: {
  tenantId: string;
  noindex?: boolean;
}) {
  const entry = getServerTenant(tenantId);
  const pathname = usePathname() ?? '/';
  const publicSite = entry.config.publicSite;
  const canonical = publicSite ? new URL(pathname, publicSite.origin).toString() : undefined;
  const publicImage = publicSite?.image
    ? {
        ...publicSite.image,
        url: new URL(publicSite.image.url, publicSite.origin).toString(),
      }
    : undefined;
  const shouldNoindex = noindex || !publicSite;

  return (
    <DefaultSeo
      {...entry.config.seo}
      canonical={canonical}
      dangerouslySetAllPagesToNoIndex={shouldNoindex}
      dangerouslySetAllPagesToNoFollow={shouldNoindex}
      openGraph={{
        ...entry.config.seo.openGraph,
        url: canonical ?? publicSite?.origin,
        images: publicImage ? [publicImage] : entry.config.seo.openGraph.images,
      }}
    />
  );
}
