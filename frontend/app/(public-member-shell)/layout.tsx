/* eslint-disable import-x/no-unused-modules */
import { getRequestTenant } from '@/lib/server/tenant';
import { getTenantUi } from '@/tenant/ui';
import { Layout } from '@/ui/Layout';
import { Providers } from '@/ui/Providers';
import type { ReactNode } from 'react';

export default async function PublicMemberShellLayout({
  children,
}: {
  children: ReactNode;
}) {
  const tenant = await getRequestTenant();
  const ui = getTenantUi(tenant.id);

  return (
    <Providers tenantId={String(tenant.id)}>
      <Layout
        includeTenantSeo={false}
        hideTopMenuIfLoggedIn
        desktopLogo={<ui.DesktopLogo />}
        mobileLogo={<ui.MobileLogo />}
        sidebarLogo={<ui.SidebarLogo />}
        socialIcons={<ui.SocialIcons />}
        footer={<ui.Footer />}
      >
        {children}
      </Layout>
    </Providers>
  );
}
