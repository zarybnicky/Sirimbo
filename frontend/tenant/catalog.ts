import type { TenantConfig } from './types';

import kometaConfig from './kometa/config.js';
import olympConfig from './olymp/config.js';
import starletConfig from './starlet/config.js';
import dynamic from 'next/dynamic';

type TenancyCatalogEntry = {
  id: number;
  name: string;
  hosts: string[];
  config: TenantConfig;
  ui: {
    DesktopLogo: React.ComponentType;
    MobileLogo: React.ComponentType;
    SidebarLogo: React.ComponentType;
    SocialIcons: React.ComponentType;
    Footer: React.ComponentType;
    Sponsors: React.ComponentType;
    TenantSeo: React.ComponentType;
  };
};

export const tenancyCatalog: [TenancyCatalogEntry, ...TenancyCatalogEntry[]] = [
  {
    id: 1,
    name: olympConfig.shortName,
    hosts: ['olymp.rozpisovnik.cz', 'tkolymp.cz'],
    config: olympConfig,
    ui: {
      DesktopLogo: dynamic(() => import('./olymp/ui').then(x => x.DesktopLogo), { ssr: false }),
      MobileLogo: dynamic(() => import('./olymp/ui').then(x => x.MobileLogo), { ssr: false }),
      SidebarLogo: dynamic(() => import('./olymp/ui').then(x => x.SidebarLogo), { ssr: false }),
      SocialIcons: dynamic(() => import('./olymp/ui').then(x => x.SocialIcons), { ssr: false }),
      Sponsors: dynamic(() => import('./olymp/ui').then(x => x.Sponsors), { ssr: false }),
      Footer: dynamic(() => import('./olymp/ui').then(x => x.Footer), { ssr: false }),
      TenantSeo: dynamic(() => import('./olymp/ui').then(x => x.TenantSeo), { ssr: false }),
    },
  },
  {
    id: 2,
    name: kometaConfig.shortName,
    hosts: ['dspkometa.rozpisovnik.cz'],
    config: kometaConfig,
    ui: {
      MobileLogo: dynamic(() => import('./kometa/ui').then(x => x.MobileLogo), { ssr: false }),
      SidebarLogo: dynamic(() => import('./kometa/ui').then(x => x.SidebarLogo), { ssr: false }),
      TenantSeo: dynamic(() => import('./kometa/ui').then(x => x.TenantSeo), { ssr: false }),
      DesktopLogo: () => null,
      SocialIcons: () => null,
      Sponsors: () => null,
      Footer: () => null,
    },
  },
  {
    id: 3,
    name: starletConfig.shortName,
    hosts: ['tkstarlet.rozpisovnik.cz'],
    config: starletConfig,
    ui: {
      MobileLogo: dynamic(() => import('./starlet/ui').then(x => x.MobileLogo), { ssr: false }),
      SidebarLogo: dynamic(() => import('./starlet/ui').then(x => x.SidebarLogo), { ssr: false }),
      TenantSeo: dynamic(() => import('./starlet/ui').then(x => x.TenantSeo), { ssr: false }),
      DesktopLogo: () => null,
      SocialIcons: () => null,
      Sponsors: () => null,
      Footer: () => null,
    },
  },
  {
    id: 4,
    name: kometaConfig.shortName,
    hosts: ['dspkometa2.rozpisovnik.cz'],
    config: kometaConfig,
    ui: {
      MobileLogo: dynamic(() => import('./kometa/ui').then(x => x.MobileLogo), { ssr: false }),
      SidebarLogo: dynamic(() => import('./kometa/ui').then(x => x.SidebarLogo), { ssr: false }),
      TenantSeo: dynamic(() => import('./kometa/ui').then(x => x.TenantSeo), { ssr: false }),
      DesktopLogo: () => null,
      SocialIcons: () => null,
      Sponsors: () => null,
      Footer: () => null,
    },
  },
];

export function getTenantUi<K extends keyof TenancyCatalogEntry['ui']>(
  tenantId: string,
  key: K,
): React.ComponentType {
  const entry = tenancyCatalog.find(x => x.id === Number.parseInt(tenantId)) || tenancyCatalog[0];
  return entry.ui[key];
}
