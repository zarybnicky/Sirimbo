/* eslint-disable import-x/no-unused-modules, tailwindcss/no-custom-classname */
import { getRequestTenant } from '@/lib/server/tenant';
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
  const { seo } = tenant.config;
  const linkTags = seo.additionalLinkTags ?? [];
  const manifest = linkTags.find((tag) => tag.rel === 'manifest')?.href;

  return {
    title: {
      default: seo.defaultTitle,
      template: seo.titleTemplate,
    },
    manifest,
    openGraph: {
      siteName: seo.openGraph.siteName,
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
        seo.additionalMetaTags
          .filter((tag) => tag.name !== 'viewport')
          .map((tag) => [tag.name, tag.content]),
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
