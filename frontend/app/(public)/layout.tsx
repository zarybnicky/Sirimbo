import type { TenantCatalogEntry } from '@/tenant/catalog';
import { getRequestTenant } from '@/tenant/server';
import { Layout } from '@/ui/Layout';
import type { ReactNode } from 'react';

/* eslint-disable import-x/no-unused-modules */
export default async function PublicLayout({ children }: { children: ReactNode }) {
  const tenant = await getRequestTenant();
  const structuredData = getTenantStructuredData(tenant);

  return (
    <>
      {structuredData.length > 0 && <JsonLd data={structuredData} />}
      <Layout showTopMenu>{children}</Layout>
    </>
  );
}

function getTenantStructuredData(tenant: TenantCatalogEntry) {
  const site = tenant.config.publicSite;
  if (!site) return [];

  const origin = tenant.config.origin;
  const organization = site.organization;

  return [
    {
      '@context': 'https://schema.org',
      '@type': 'SportsOrganization',
      '@id': `${origin}/#organization`,
      name: tenant.name,
      legalName: organization.legalName,
      url: origin,
      logo: new URL(organization.logo, origin).toString(),
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
      '@id': `${origin}/#website`,
      name: tenant.name,
      url: origin,
      inLanguage: 'cs-CZ',
      publisher: {
        '@id': `${origin}/#organization`,
      },
    },
  ];
}

type JsonLdValue =
  | string
  | number
  | boolean
  | null
  | undefined
  | JsonLdValue[]
  | { [key: string]: JsonLdValue };

function JsonLd({ data }: { data: JsonLdValue | JsonLdValue[] }) {
  return (
    <script
      type="application/ld+json"
      dangerouslySetInnerHTML={{
        __html: JSON.stringify(data).replaceAll('<', String.raw`\u003c`),
      }}
    />
  );
}
