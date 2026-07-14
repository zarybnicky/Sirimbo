/* eslint-disable import-x/no-unused-modules, tailwindcss/no-custom-classname */
import { getRequestTenant } from '@/tenant/server';
import type { Metadata, Viewport } from 'next';
import { NuqsAdapter } from 'nuqs/adapters/next/app';
import type { ReactNode } from 'react';

import 'nprogress/nprogress.css';
import 'react-data-grid/lib/styles.css';
import 'react-toastify/dist/ReactToastify.css';
import '../style/calendar.css';
import '../style/index.css';
import '../style/leaflet.css';
import '../style/lite-youtube-embed.css';

export async function generateMetadata(): Promise<Metadata> {
  const tenant = await getRequestTenant();
  const { seo, publicSite, origin } = tenant.config;
  const linkTags = seo.additionalLinkTags ?? [];

  return {
    metadataBase: publicSite ? new URL(origin) : undefined,
    title: {
      default: seo.defaultTitle,
      template: seo.titleTemplate,
    },
    description: seo.description,
    applicationName: tenant.name,
    manifest: linkTags.find((tag) => tag.rel === 'manifest')?.href,
    openGraph: {
      siteName: tenant.name,
      type: 'website',
      locale: 'cs_CZ',
      description: seo.description,
      images: publicSite?.image ? [publicSite?.image] : [],
    },
    twitter: publicSite?.image
      ? {
          card: 'summary_large_image',
          title: seo.defaultTitle,
          description: seo.description,
          images: [publicSite?.image.url],
        }
      : undefined,
    robots: publicSite
      ? {
          index: true,
          follow: true,
          googleBot: {
            index: true,
            follow: true,
            'max-image-preview': 'large',
            'max-snippet': -1,
            'max-video-preview': -1,
          },
        }
      : {
          index: false,
          follow: false,
          googleBot: {
            index: false,
            follow: false,
          },
        },
    facebook: seo.facebook?.appId ? { appId: seo.facebook.appId } : undefined,
    icons: {
      icon: linkTags
        .filter((tag) => tag.rel === 'icon')
        .map((tag) => ({ url: tag.href, sizes: tag.sizes })),
      shortcut: linkTags
        .filter((tag) => tag.rel === 'shortcut icon')
        .map((tag) => tag.href),
      apple: linkTags
        .filter((tag) => tag.rel === 'apple-touch-icon')
        .map((tag) => ({ url: tag.href, sizes: tag.sizes })),
      other: linkTags
        .filter((tag) => tag.rel === 'mask-icon')
        .map((tag) => ({
          rel: tag.rel,
          url: tag.href,
          color: tag.color,
        })),
    },
    other: {
      ...Object.fromEntries(
        (seo.additionalMetaTags || []).map((tag) => [tag.name, tag.content]),
      ),
    },
  };
}

export async function generateViewport(): Promise<Viewport> {
  const tenant = await getRequestTenant();

  return {
    themeColor: tenant.config.seo.themeColor,
  };
}

export default async function RootLayout({ children }: { children: ReactNode }) {
  const tenant = await getRequestTenant();

  return (
    <html lang="cs" className={`tenant-${tenant.id}`}>
      <body className={`tenant-${tenant.id}`}>
        <NuqsAdapter>{children}</NuqsAdapter>
      </body>
    </html>
  );
}
