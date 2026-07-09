import type { Metadata, MetadataRoute } from 'next';
import type { ServerTenantCatalogEntry } from '@/tenant/catalog-server';
import type { TenantPublicSiteConfig } from '@/tenant/types';

export type PublicSitemapRoute = {
  path: string;
  changeFrequency: MetadataRoute.Sitemap[number]['changeFrequency'];
  priority: number;
};

export const publicRoutes: PublicSitemapRoute[] = [
  { path: '/', changeFrequency: 'weekly', priority: 1 },
  { path: '/clanky', changeFrequency: 'daily', priority: 0.8 },
  { path: '/akce', changeFrequency: 'daily', priority: 0.8 },
  { path: '/treninkove-programy', changeFrequency: 'monthly', priority: 0.8 },
  { path: '/treninkove-skupiny', changeFrequency: 'monthly', priority: 0.8 },
  { path: '/o-nas', changeFrequency: 'monthly', priority: 0.7 },
  { path: '/treneri', changeFrequency: 'monthly', priority: 0.7 },
  { path: '/kde-trenujeme', changeFrequency: 'monthly', priority: 0.7 },
  { path: '/vyhody-clenstvi', changeFrequency: 'monthly', priority: 0.7 },
  { path: '/skolni-krouzky', changeFrequency: 'monthly', priority: 0.7 },
  { path: '/vystoupeni', changeFrequency: 'monthly', priority: 0.7 },
  { path: '/galerie', changeFrequency: 'monthly', priority: 0.6 },
  { path: '/galerie-mistru', changeFrequency: 'monthly', priority: 0.6 },
  { path: '/kontakt', changeFrequency: 'monthly', priority: 0.6 },
  { path: '/registrace', changeFrequency: 'weekly', priority: 0.5 },
  { path: '/ochrana-osobnich-udaju', changeFrequency: 'yearly', priority: 0.2 },
];

export function stripHtml(value: string | null | undefined) {
  return (value ?? '')
    .replaceAll(/<[^>]*>/g, ' ')
    .replaceAll('&nbsp;', ' ')
    .replaceAll(/\s+/g, ' ')
    .trim();
}

export function absoluteTenantUrl(tenant: ServerTenantCatalogEntry, path = '/') {
  const origin = tenant.config.publicSite?.origin ?? `https://${tenant.hosts[0]}`;
  return new URL(path, origin).toString();
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

export function getTenantStructuredData(tenant: ServerTenantCatalogEntry) {
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
