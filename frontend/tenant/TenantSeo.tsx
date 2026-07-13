'use client';

import { getServerTenant } from '@/tenant/catalog';
import { tenantIdAtom } from '@/ui/state/auth';
import { useAtomValue } from 'jotai';
import { DefaultSeo, NextSeo } from 'next-seo';
import { usePathname } from 'next/navigation';

export function TenantSeo() {
  const tenantId = useAtomValue(tenantIdAtom);

  const entry = getServerTenant(tenantId);
  const pathname = usePathname() ?? '/';
  const publicSite = entry.config.publicSite;
  const canonical = publicSite
    ? new URL(pathname, publicSite.origin).toString()
    : undefined;
  const publicImage = publicSite?.image
    ? {
        ...publicSite.image,
        url: new URL(publicSite.image.url, publicSite.origin).toString(),
      }
    : undefined;
  return (
    <DefaultSeo
      {...entry.config.seo}
      canonical={canonical}
      openGraph={{
        ...entry.config.seo.openGraph,
        url: canonical ?? publicSite?.origin,
        images: publicImage ? [publicImage] : entry.config.seo.openGraph.images,
      }}
    />
  );
}
