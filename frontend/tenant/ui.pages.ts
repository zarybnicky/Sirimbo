import dynamic from 'next/dynamic';
import React from 'react';
import { defaultTenant } from '@/tenant/catalog';

type TenantUI = {
  DesktopLogo: React.ComponentType;
  MobileLogo: React.ComponentType;
  SidebarLogo: React.ComponentType;
  SocialIcons: React.ComponentType;
  Footer: React.ComponentType;
  Sponsors: React.ComponentType;
};

const tenantUi: Record<number, TenantUI> = {
  1: {
    DesktopLogo: dynamic(() => import('./olymp/ui').then((x) => x.DesktopLogo), {
      ssr: false,
    }),
    MobileLogo: dynamic(() => import('./olymp/ui').then((x) => x.MobileLogo), {
      ssr: false,
    }),
    SidebarLogo: dynamic(() => import('./olymp/ui').then((x) => x.SidebarLogo), {
      ssr: false,
    }),
    SocialIcons: dynamic(() => import('./olymp/ui').then((x) => x.SocialIcons), {
      ssr: false,
    }),
    Sponsors: dynamic(() => import('./olymp/ui').then((x) => x.Sponsors), {
      ssr: false,
    }),
    Footer: dynamic(() => import('./olymp/ui').then((x) => x.Footer), { ssr: false }),
  },
  2: {
    MobileLogo: dynamic(() => import('./kometa/ui').then((x) => x.MobileLogo), {
      ssr: false,
    }),
    SidebarLogo: dynamic(() => import('./kometa/ui').then((x) => x.SidebarLogo), {
      ssr: false,
    }),
    DesktopLogo: () => null,
    SocialIcons: () => null,
    Sponsors: () => null,
    Footer: () => null,
  },
  3: {
    MobileLogo: dynamic(() => import('./starlet/ui').then((x) => x.MobileLogo), {
      ssr: false,
    }),
    SidebarLogo: dynamic(() => import('./starlet/ui').then((x) => x.SidebarLogo), {
      ssr: false,
    }),
    DesktopLogo: () => null,
    SocialIcons: () => null,
    Sponsors: () => null,
    Footer: () => null,
  },
  4: {
    MobileLogo: dynamic(() => import('./kometa/ui').then((x) => x.MobileLogo), {
      ssr: false,
    }),
    SidebarLogo: dynamic(() => import('./kometa/ui').then((x) => x.SidebarLogo), {
      ssr: false,
    }),
    DesktopLogo: () => null,
    SocialIcons: () => null,
    Sponsors: () => null,
    Footer: () => null,
  },
};

export function getTenantUi<K extends keyof TenantUI>(
  tenantId: string,
  key: K,
): React.ComponentType {
  let id = Number.parseInt(tenantId, 10);
  if (!Number.isFinite(id) || !(id in tenantUi)) id = defaultTenant.id;
  return tenantUi[id]![key];
}
