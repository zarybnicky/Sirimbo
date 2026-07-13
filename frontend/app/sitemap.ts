/* eslint-disable import-x/no-unused-modules */
import { ArticlesDocument } from '@/graphql/Articles';
import { CohortGroupListDocument } from '@/graphql/CohortGroup';
import { CohortListDocument } from '@/graphql/Cohorts';
import { getRequestTenant } from '@/lib/tenant/server';
import { executeGraphql } from '@/lib/server/graphql';
import { absoluteTenantUrl, publicRoutes } from '@/lib/seo';
import { slugify } from '@/lib/slugify';
import type { MetadataRoute } from 'next';

export const dynamic = 'force-dynamic';

async function optional<T>(promise: Promise<T>): Promise<T | null> {
  try {
    return await promise;
  } catch {
    return null;
  }
}

export default async function sitemap(): Promise<MetadataRoute.Sitemap> {
  const tenant = await getRequestTenant();

  if (!tenant.config.publicSite) {
    return [];
  }

  const [articles, cohortGroups, cohorts] = await Promise.all([
    optional(
      executeGraphql(ArticlesDocument, {
        first: 1000,
        offset: 0,
        visibleOnly: true,
      }),
    ),
    optional(
      executeGraphql(CohortGroupListDocument, {
        first: 100,
        offset: 0,
        isPublic: true,
      }),
    ),
    optional(
      executeGraphql(CohortListDocument, {
        first: 500,
        offset: 0,
        visible: true,
        archived: false,
      }),
    ),
  ]);

  return [
    ...publicRoutes.map((route) => ({
      url: absoluteTenantUrl(tenant, route.path),
      changeFrequency: route.changeFrequency,
      priority: route.priority,
    })),
    ...(articles?.aktualities?.nodes ?? []).map((article) => ({
      url: absoluteTenantUrl(tenant, `/clanky/${article.id}/${slugify(article.atJmeno)}`),
      lastModified: article.updatedAt ?? article.createdAt ?? undefined,
      changeFrequency: 'monthly' as const,
      priority: 0.65,
      images: article.titlePhotoUrl ? [article.titlePhotoUrl] : undefined,
    })),
    ...(cohortGroups?.cohortGroups?.nodes ?? []).map((group) => ({
      url: absoluteTenantUrl(
        tenant,
        `/treninkove-programy/${group.id}/${slugify(group.name)}`,
      ),
      changeFrequency: 'monthly' as const,
      priority: 0.65,
    })),
    ...(cohorts?.cohortsList ?? []).map((cohort) => ({
      url: absoluteTenantUrl(
        tenant,
        `/treninkove-skupiny/${cohort.id}/${slugify(cohort.name)}`,
      ),
      changeFrequency: 'monthly' as const,
      priority: 0.55,
    })),
  ];
}
