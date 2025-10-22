import dynamic from 'next/dynamic';
import { serverTenantCatalog, ServerTenantCatalogEntry } from './catalog-server';

type TenantCatalogEntry = ServerTenantCatalogEntry & {
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

export const tenantCatalog: Record<number, TenantCatalogEntry> = {
  1: {
    ...serverTenantCatalog[1],
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
  2: {
    ...serverTenantCatalog[2],
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
  3: {
    ...serverTenantCatalog[3],
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
  4: {
    ...serverTenantCatalog[4],
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
};

export function getTenantUi<K extends keyof TenantCatalogEntry['ui']>(
  tenantId: string,
  key: K,
): React.ComponentType {
  const entry = tenantCatalog[Number.parseInt(tenantId)] || tenantCatalog[1]!;
  return entry.ui[key];
}
