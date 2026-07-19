'use client';

import { useTenantConfig } from '@/ui/state/auth';
import { DefaultSeo } from 'next-seo';

export function TenantSeo() {
  const { origin, publicSite, seo } = useTenantConfig();
  // FIXME: const canonical = new URL(pathname, origin).toString();

  return (
    <DefaultSeo
      {...seo}
      additionalMetaTags={[
        ...(seo.additionalMetaTags ?? []),
        { name: 'viewport', content: 'initial-scale=1,width=device-width' },
      ]}
      /* canonical={canonical} */
      openGraph={{
        ...seo.openGraph,
        // url: canonical,
        images: [
          ...(publicSite?.image
            ? [
                {
                  ...publicSite.image,
                  url: new URL(publicSite.image.url, origin).toString(),
                },
              ]
            : []),
        ],
      }}
    />
  );
}
