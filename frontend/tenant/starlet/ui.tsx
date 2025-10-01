import React from 'react';
import tenantConfig from './config.js';
import { DefaultSeo } from 'next-seo';
import Link from 'next/link';
import Image from 'next/image';
import LogoIcon from './logo-white-no-text.png';
import LogoMobile from './starlet-logo-mobile.png';

export function TenantSeo() {
  return (
    <DefaultSeo
      titleTemplate={`%s · ${tenantConfig.shortName}`}
      defaultTitle={tenantConfig.shortName}
      themeColor="#000"
      openGraph={{ siteName: tenantConfig.shortName }}
      additionalMetaTags={[
        { name: 'viewport', content: 'initial-scale=1,width=device-width' },
      ]}
    />
  );
  /* additionalLinkTags={[
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
    ]} */
}

export function SocialIcons() {
  return null;
}

export function Sponsors() {
  return null;
}

export function SidebarLogo() {
  return (
    <div className="hidden lg:flex">
      <Link href="/dashboard" className="h-28 pt-3 pl-2 pr-4 mx-auto">
        <Image
          alt=""
          src={LogoIcon}
          priority
          sizes="100vw"
          style={{
            width: "100%",
            height: "auto"
          }} />
      </Link>
    </div>
  );
}

export function DesktopLogo() {
  return null;
}

export function MobileLogo() {
  return (
    <Image
      className="pb-1 pr-4 -mt-1"
      alt=""
      src={LogoIcon}
      height="65"
      priority
      style={{
        maxWidth: "100%",
        height: "auto"
      }} />
  );
}
