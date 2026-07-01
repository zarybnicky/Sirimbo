/* eslint-disable import-x/no-unused-modules */
import { getRequestTenant } from '@/lib/server/tenant';
import { getTenantUi } from '@/tenant/ui';
import { Providers } from '@/ui/Providers';
import { Layout as AppLayout } from '@/ui/Layout.app';
import type { ReactNode } from 'react';

export default async function PublicLayout({ children }: { children: ReactNode }) {
  const tenant = await getRequestTenant();
  const ui = getTenantUi(tenant.id);

  return (
    <Providers tenantId={String(tenant.id)}>
      <AppLayout
        showTopMenu
        desktopLogo={<ui.DesktopLogo />}
        mobileLogo={<ui.MobileLogo />}
        sidebarLogo={<ui.SidebarLogo />}
        socialIcons={<ui.SocialIcons />}
        footer={<ui.Footer />}
      >
        {children}
      </AppLayout>
    </Providers>
  );
}
