import 'server-only';

import { getRequestTenant } from '@/lib/tenant/server';
import type { TenantPublicSiteConfig } from '@/tenant/types';
import { Metadata } from 'next';

export async function publicPageMetadata({
  title,
  description,
  path,
  image,
}: {
  title: string;
  description: string;
  path: string;
  image?: TenantPublicSiteConfig['image'];
}): Promise<Metadata> {

  if (!image) {
    const tenant = await getRequestTenant();
    image = tenant.config.publicSite?.image;
  }

  return {
    title,
    description,
    alternates: {
      canonical: path,
    },
    openGraph: {
      title,
      description,
      type: 'website',
      url: path,
      images: image ? [image] : undefined,
    },
    twitter: {
      card: 'summary_large_image',
      title,
      description,
      images: image ? [image.url] : undefined,
    },
  };
}
