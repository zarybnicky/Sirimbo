/* eslint-disable import-x/no-unused-modules */
import { getRequestTenant } from '@/lib/server/tenant';
import { absoluteTenantUrl } from '@/lib/seo';
import type { MetadataRoute } from 'next';

export const dynamic = 'force-dynamic';

const privatePaths = [
  '/admin/',
  '/api/',
  '/aktuality/',
  '/clenove/',
  '/crm',
  '/dashboard',
  '/federated/',
  '/graphql',
  '/graphiql',
  '/login',
  '/member/',
  '/nastenka/',
  '/otp',
  '/pary/',
  '/platby',
  '/pozvanky',
  '/profil',
  '/rozpis',
  '/rpc/',
  '/starlet-import/',
  '/tanecni-klub',
  '/tattletale',
  '/upload',
  '/users/',
  '/zapomenute-heslo',
  '/zebricek',
];

export default async function robots(): Promise<MetadataRoute.Robots> {
  const tenant = await getRequestTenant();

  if (!tenant.config.publicSite) {
    return {
      rules: {
        userAgent: '*',
        disallow: '/',
      },
    };
  }

  return {
    rules: {
      userAgent: '*',
      allow: '/',
      disallow: privatePaths,
    },
    sitemap: absoluteTenantUrl(tenant, '/sitemap.xml'),
    host: new URL(absoluteTenantUrl(tenant)).host,
  };
}
