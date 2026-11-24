import { loadCurrentUser } from '@/lib/server/session';
import { getRequestContext, getTenants } from '@/lib/server/tenant';
import { Providers } from '@/ui/Providers';
import type { Metadata, Viewport } from 'next';
import { cookies } from 'next/headers';
import { NuqsAdapter } from 'nuqs/adapters/next/app';
import type { ReactNode } from 'react';
import { isTruthy } from '@/lib/truthyFilter';
import { UI_COOKIE } from '@/lib/ui.ts';

import '../style/index.css';
import 'react-lite-youtube-embed/dist/LiteYouTubeEmbed.css';

export async function generateMetadata(): Promise<Metadata> {
  const {
    tenant: { name, config },
  } = await getRequestContext();
  const { seo, publicSite, origin } = config;
  const linkTags = seo.additionalLinkTags ?? [];

  return {
    metadataBase: new URL(origin),
    title: {
      default: seo.defaultTitle,
      template: seo.titleTemplate,
    },
    description: seo.description,
    applicationName: name,
    manifest: linkTags.find((tag) => tag.rel === 'manifest')?.href,
    openGraph: {
      siteName: name,
      type: 'website',
      locale: 'cs_CZ',
      description: seo.description,
      images: publicSite?.image ? [publicSite?.image] : [],
    },
    twitter: publicSite?.image
      ? {
          card: 'summary_large_image',
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
  const { tenant } = await getRequestContext();

  return {
    themeColor: tenant.config.seo.themeColor,
  };
}

export default async function RootLayout({ children }: { children: ReactNode }) {
  const [{ tenant }, { auth, stale }, cookieStore, tenants] = await Promise.all([
    getRequestContext(),
    loadCurrentUser(),
    cookies(),
    getTenants(),
  ]);

  const palette = tenants.map((tenant) =>
    [tenant.accent_light, tenant.accent_dark]
      .filter(isTruthy)
      .map((values, mode) => {
        const scale = values
          .map((value, i) => `--color-accent-${i + 1}:${value};`)
          .join('');
        const rule = `[data-tenant='${tenant.id}']{${scale}}`;
        return mode === 1 ? `@media(prefers-color-scheme:dark){${rule}}` : rule;
      })
      .join(''),
  );

  return (
    <html lang="cs" data-tenant={tenant.id} suppressHydrationWarning>
      <head>
        <style>{palette.join('')}</style>
      </head>
      <body suppressHydrationWarning>
        <NuqsAdapter>
          <Providers
            initialAuth={auth}
            initialSessionStale={stale}
            initialTenant={tenant}
            initialUiCookie={cookieStore.get(UI_COOKIE)?.value}
          >
            {children}
          </Providers>
        </NuqsAdapter>
      </body>
    </html>
  );
}
