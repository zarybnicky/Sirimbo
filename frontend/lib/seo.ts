import type { Metadata } from 'next';
import type { TenantCatalogEntry } from '@/tenant/catalog';
import type { TenantPublicSiteConfig } from '@/tenant/types';

export function stripHtml(value: string | null | undefined) {
  return (value ?? '')
    .replaceAll(/<[^>]*>/g, ' ')
    .replaceAll('&nbsp;', ' ')
    .replaceAll(/\s+/g, ' ')
    .trim();
}

export function createPublicPageMetadata({
  title,
  description,
  path,
  image,
}: {
  title: string;
  description: string;
  path: string;
  image?: TenantPublicSiteConfig['image'];
}): Metadata {
  return {
    title,
    description,
    alternates: {
      canonical: path,
    },
    openGraph: {
      title,
      description,
      type: 'website',
      url: path,
      images: image ? [image] : undefined,
    },
    twitter: {
      card: 'summary_large_image',
      title,
      description,
      images: image ? [image.url] : undefined,
    },
  };
}

export function getTenantStructuredData(tenant: TenantCatalogEntry) {
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
