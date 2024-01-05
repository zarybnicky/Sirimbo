import React from 'react';
import tenantConfig from './config.js';
import { DefaultSeo } from 'next-seo';
import Link from 'next/link';
import Image from 'next/image';
import LogoOnDark from './logo-white.webp';
import LogoOnWhite from './logo-vec.webp';

export const TenantSeo = () => (
  <DefaultSeo
    titleTemplate={`%s · ${tenantConfig.shortName}`}
    defaultTitle={tenantConfig.shortName}
    themeColor="#000"
    openGraph={{ siteName: tenantConfig.shortName }}
    additionalMetaTags={[
      { name: 'viewport', content: 'initial-scale=1,width=device-width' },
    ]}
    additionalLinkTags={[
      {
        rel: 'apple-touch-icon',
        sizes: '180x180',
        href: '/kometa/apple-touch-icon.png?v=3',
      },
      { rel: 'icon', sizes: '32x32', href: '/kometa/favicon-32x32.png?v=3' },
      { rel: 'icon', sizes: '16x16', href: '/kometa/favicon-16x16.png?v=3' },
      { rel: 'shortcut icon', href: '/kometa/favicon.ico?v=3' },
      { rel: 'manifest', href: '/kometa/site.webmanifest?v=3' },
      {
        rel: 'mask-icon',
        color: '#da532c',
        href: '/kometa/safari-pinned-tab.svg?v=3',
      },
    ]}
  />
);

export function SocialIcons() {
  return null;
}

export function Sponsors() {
  return null;
}

export function SidebarLogo() {
  return (
    <div className="hidden lg:flex">
      <Link href="/dashboard" className="h-16 mt-3 mx-auto">
        <Image
          alt=""
          src={LogoOnDark}
          style={{
            width: 'auto',
            height: '100%',
          }}
          priority
        />
      </Link>
    </div>
  );
}

export function DesktopLogo() {
  return null;
}

export function MobileLogo() {
  return (
    <Image alt="" src={LogoOnWhite} height="50" priority />
  );
}
