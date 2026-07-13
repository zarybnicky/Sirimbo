/* eslint-disable import-x/no-unused-modules */
import { ArticlesDocument } from '@/graphql/Articles';
import { CohortGroupListDocument } from '@/graphql/CohortGroup';
import { CohortListDocument } from '@/graphql/Cohorts';
import { getRequestTenant } from '@/lib/tenant/server';
import { executeGraphql } from '@/lib/server/graphql';
import { slugify } from '@/lib/slugify';
import type { MetadataRoute } from 'next';

export const dynamic = 'force-dynamic';

export type PublicSitemapRoute = {
  path: string;
  changeFrequency: MetadataRoute.Sitemap[number]['changeFrequency'];
  priority: number;
};

const publicRoutes: PublicSitemapRoute[] = [
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

export default async function sitemap(): Promise<MetadataRoute.Sitemap> {
  const tenant = await getRequestTenant();

  if (!tenant.config.publicSite) {
    return [];
  }
  const origin = tenant.config.publicSite?.origin ?? `https://${tenant.hosts[0]}`;

  const [articles, cohortGroups, cohorts] = await Promise.all([
    executeGraphql(ArticlesDocument, {
      first: 1000,
      offset: 0,
      visibleOnly: true,
    }).catch(() => null),
    executeGraphql(CohortGroupListDocument, {
      first: 100,
      offset: 0,
      isPublic: true,
    }).catch(() => null),
    executeGraphql(CohortListDocument, {
      first: 500,
      offset: 0,
      visible: true,
      archived: false,
    }).catch(() => null),
  ]);

  return [
    ...publicRoutes.map((route) => ({
      url: new URL(route.path, origin).toString(),
      changeFrequency: route.changeFrequency,
      priority: route.priority,
    })),
    ...(articles?.aktualities?.nodes ?? []).map((article) => ({
      url: new URL(
        `/clanky/${article.id}/${slugify(article.atJmeno)}`,
        origin,
      ).toString(),
      lastModified: article.updatedAt ?? article.createdAt ?? undefined,
      changeFrequency: 'monthly' as const,
      priority: 0.65,
      images: article.titlePhotoUrl ? [article.titlePhotoUrl] : undefined,
    })),
    ...(cohortGroups?.cohortGroups?.nodes ?? []).map((group) => ({
      url: new URL(
        `/treninkove-programy/${group.id}/${slugify(group.name)}`,
        origin,
      ).toString(),
      changeFrequency: 'monthly' as const,
      priority: 0.65,
    })),
    ...(cohorts?.cohortsList ?? []).map((cohort) => ({
      url: new URL(
        `/treninkove-skupiny/${cohort.id}/${slugify(cohort.name)}`,
        origin,
      ).toString(),
      changeFrequency: 'monthly' as const,
      priority: 0.55,
    })),
  ];
}
