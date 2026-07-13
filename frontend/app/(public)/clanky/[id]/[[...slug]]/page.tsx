/* eslint-disable import-x/no-unused-modules */
import { ArticleDocument } from '@/graphql/Articles';
import { executeGraphql } from '@/lib/server/graphql';
import { getRequestTenant } from '@/lib/tenant/server';
import { stripHtml } from '@/lib/seo';
import { slugify } from '@/lib/slugify';
import { fullDateFormatter } from '@/ui/format';
import { JsonLd } from '@/ui/JsonLd';
import { RichTextView } from '@/ui/RichTextView';
import { PageHeader } from '@/ui/TitleBar';
import type { Metadata } from 'next';
import { notFound, redirect } from 'next/navigation';
import { cache } from 'react';

type ArticlePageProps = {
  params: Promise<{
    id: string;
    slug?: string[];
  }>;
};

const getArticle = cache(async (id: string) => {
  if (!/^\d+$/.test(id)) return null;
  return executeGraphql(ArticleDocument, { id }).then((x) => x.aktuality);
});

export async function generateMetadata({
  params,
}: ArticlePageProps): Promise<Metadata> {
  const { id } = await params;
  const item = await getArticle(id);
  if (!item) {
    return {
      title: 'Článek',
    };
  }
  const description = stripHtml(item.atPreview);
  const canonicalPath = `/clanky/${item.id}/${slugify(item.atJmeno)}`;

  return {
    title: item.atJmeno,
    description,
    alternates: {
      canonical: canonicalPath,
    },
    openGraph: {
      type: 'article',
      title: item.atJmeno,
      description,
      url: canonicalPath,
      publishedTime: item.createdAt ?? undefined,
      modifiedTime: item.updatedAt ?? undefined,
      images: item.titlePhotoUrl
        ? [{ url: item.titlePhotoUrl, alt: item.atJmeno }]
        : undefined,
    },
    twitter: {
      card: 'summary_large_image',
      title: item.atJmeno,
      description,
      images: item.titlePhotoUrl ? [item.titlePhotoUrl] : undefined,
    },
    robots: item.isVisible ? undefined : { index: false, follow: false },
  };
}

export default async function ArticlePage({ params }: ArticlePageProps) {
  const { id, slug } = await params;
  const item = await getArticle(id);

  if (!item) notFound();
  if (slug?.join('/') !== slugify(item.atJmeno)) {
    redirect(`/clanky/${item.id}/${slugify(item.atJmeno)}`);
  }

  const tenant = await getRequestTenant();
  const site = tenant.config.publicSite;
  const canonicalPath = `/clanky/${item.id}/${slugify(item.atJmeno)}`;
  const description = stripHtml(item.atPreview);
  const articleJsonLd = site
    ? {
        '@context': 'https://schema.org',
        '@type': 'Article',
        headline: item.atJmeno,
        description,
        image: item.titlePhotoUrl ? [item.titlePhotoUrl] : undefined,
        datePublished: item.createdAt ?? undefined,
        dateModified: item.updatedAt ?? item.createdAt ?? undefined,
        author: item.atKdo
          ? {
              '@type': 'Person',
              name: item.atKdo,
            }
          : {
              '@id': `${site.origin}/#organization`,
            },
        publisher: {
          '@id': `${site.origin}/#organization`,
        },
        mainEntityOfPage: new URL(canonicalPath, site.origin).toString(),
        inLanguage: site.locale,
      }
    : null;

  return (
    <>
      {articleJsonLd && <JsonLd data={articleJsonLd} />}
      <PageHeader title={item.atJmeno} />
      {item.createdAt && (
        <div className="text-neutral-11 mb-6 -mt-4">
          {fullDateFormatter.format(new Date(item.createdAt))}
        </div>
      )}
      <RichTextView value={item.atText} />
    </>
  );
}
