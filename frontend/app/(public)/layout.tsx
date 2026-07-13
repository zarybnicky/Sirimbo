import { getRequestTenant } from '@/lib/tenant/server';
import type { TenantCatalogEntry } from '@/tenant/catalog';
import { getTenantUi } from '@/tenant/ui';
import { JsonLd } from '@/ui/JsonLd';
import { Providers } from '@/ui/Providers';
import { Layout } from '@/ui/Layout';
import type { ReactNode } from 'react';

/* eslint-disable import-x/no-unused-modules */
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

function getTenantStructuredData(tenant: TenantCatalogEntry) {
  const site = tenant.config.publicSite;
  if (!site) return [];

  const organizationId = `${site.origin}/#organization`;
  const organization = site.organization;

  return [
    {
      '@context': 'https://schema.org',
      '@type': 'SportsOrganization',
      '@id': organizationId,
      name: organization.name,
      legalName: organization.legalName,
      url: site.origin,
      logo: new URL(organization.logo, site.origin).toString(),
      email: organization.email,
      telephone: organization.telephone,
      sameAs: organization.sameAs,
      address: organization.address
        ? {
            '@type': 'PostalAddress',
            ...organization.address,
          }
        : undefined,
    },
    {
      '@context': 'https://schema.org',
      '@type': 'WebSite',
      '@id': `${site.origin}/#website`,
      name: tenant.name,
      url: site.origin,
      inLanguage: site.locale,
      publisher: {
        '@id': organizationId,
      },
    },
  ];
}
