'use client';

import { useTenantConfig } from '@/ui/state/auth';
import { DefaultSeo } from 'next-seo';
import { usePathname } from 'next/navigation';

export function TenantSeo() {
  const { origin, publicSite, seo } = useTenantConfig();
  const pathname = usePathname() ?? '/';
  const canonical = new URL(pathname, origin).toString();
  return (
    <DefaultSeo
      {...seo}
      canonical={canonical}
      openGraph={{
        ...seo.openGraph,
        url: canonical,
        images: publicSite?.image
          ? [
              {
                ...publicSite.image,
                url: new URL(publicSite.image.url, origin).toString(),
              },
            ]
          : seo.openGraph.images,
      }}
    />
  );
}
