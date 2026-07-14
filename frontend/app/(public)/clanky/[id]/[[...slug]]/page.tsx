/* eslint-disable import-x/no-unused-modules */
import { ArticleDocument } from '@/graphql/Articles';
import { executeGraphql } from '@/lib/server/graphql';
import { getRequestTenant } from '@/tenant/server';
import { slugify } from '@/lib/slugify';
import { fullDateFormatter } from '@/ui/format';
import { RichTextView } from '@/ui/RichTextView';
import { PageHeader } from '@/ui/TitleBar';
import type { Metadata } from 'next';
import { notFound, redirect } from 'next/navigation';
import { cache } from 'react';
import { stripHtml } from '@/lib/stripHtml';

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

function createSeoDescription(
  primary: string | null | undefined,
  fallback: string | null | undefined,
  maxLength = 160,
) {
  const value = stripHtml(primary) || stripHtml(fallback);
  if (value.length <= maxLength) return value;

  const shortened = value.slice(0, maxLength - 1);
  const lastSpace = shortened.lastIndexOf(' ');
  const end = lastSpace >= maxLength * 0.75 ? lastSpace : shortened.length;
  return `${shortened.slice(0, end)}…`;
}

export async function generateMetadata({ params }: ArticlePageProps): Promise<Metadata> {
  const { id } = await params;
  const [item, tenant] = await Promise.all([getArticle(id), getRequestTenant()]);
  if (!item) {
    return {
      title: 'Článek',
    };
  }
  const description = createSeoDescription(item.atPreview, item.atText);
  const canonicalPath = `/clanky/${item.id}/${slugify(item.atJmeno)}`;
  const image = item.titlePhotoUrl
    ? { url: item.titlePhotoUrl, alt: item.atJmeno }
    : tenant.config.publicSite?.image;

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
      images: image ? [image] : undefined,
    },
    twitter: {
      card: 'summary_large_image',
      title: item.atJmeno,
      description,
      images: image ? [image.url] : undefined,
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

  return (
    <>
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
