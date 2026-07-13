/* eslint-disable import-x/no-unused-modules */
import { getRequestTenant } from '@/lib/tenant/server';
import { getTenantStructuredData } from '@/lib/seo';
import { getTenantUi } from '@/tenant/ui';
import { JsonLd } from '@/ui/JsonLd';
import { Providers } from '@/ui/Providers';
import { Layout } from '@/ui/Layout';
import type { ReactNode } from 'react';

export default async function PublicLayout({ children }: { children: ReactNode }) {
  const tenant = await getRequestTenant();
  const ui = getTenantUi(tenant.id);
  const structuredData = getTenantStructuredData(tenant);

  return (
    <Providers>
      {structuredData.length > 0 && <JsonLd data={structuredData} />}
      <Layout
        includeTenantSeo={false}
        showTopMenu
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
