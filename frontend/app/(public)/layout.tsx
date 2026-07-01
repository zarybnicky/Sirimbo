/* eslint-disable import-x/no-unused-modules */
import { getRequestTenant } from '@/lib/server/tenant';
import { getAppTenantUi } from '@/tenant/catalog-app';
import type { ReactNode } from 'react';
import { PublicChrome } from './PublicChrome';

export default async function PublicLayout({ children }: { children: ReactNode }) {
  const tenant = await getRequestTenant();
  const ui = getAppTenantUi(tenant.id);

  return (
    <PublicChrome
      tenantId={String(tenant.id)}
      desktopLogo={<ui.DesktopLogo />}
      mobileLogo={<ui.MobileLogo />}
      sidebarLogo={<ui.SidebarLogo />}
      socialIcons={<ui.SocialIcons />}
      footer={<ui.Footer />}
    >
      {children}
    </PublicChrome>
  );
}
