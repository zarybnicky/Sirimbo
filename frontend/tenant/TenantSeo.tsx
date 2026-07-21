'use client';

import { useTenantConfig } from '@/ui/state/auth';
import { DefaultSeo } from 'next-seo';

export function TenantSeo() {
  const { seo } = useTenantConfig();

  return (
    <DefaultSeo
      {...seo}
      additionalMetaTags={[
        ...(seo.additionalMetaTags ?? []),
        { name: 'viewport', content: 'initial-scale=1,width=device-width' },
      ]}
      openGraph={{
        ...seo.openGraph,
      }}
    />
  );
}
