/* eslint-disable import-x/no-unused-modules */
import { ArticleDocument } from '@/graphql/Articles';
import { executeGraphql } from '@/lib/server/graphql';
import { slugify } from '@/lib/slugify';
import { fullDateFormatter } from '@/ui/format';
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

  const data = await executeGraphql(ArticleDocument, { id });
  return data.aktuality;
});

function articlePath(id: string, title: string) {
  const slug = slugify(title);
  return slug ? `/clanky/${id}/${slug}` : `/clanky/${id}`;
}

function metadataDescription(value: string) {
  return value
    .replaceAll(/<[^>]*>/g, ' ')
    .replaceAll(/\s+/g, ' ')
    .trim();
}

export async function generateMetadata({
  params,
}: ArticlePageProps): Promise<Metadata> {
  const { id } = await params;
  const article = await getArticle(id);

  if (!article) {
    return {
      title: 'Článek',
    };
  }

  const description = metadataDescription(article.atPreview);
  const canonical = articlePath(article.id, article.atJmeno);

  return {
    title: article.atJmeno,
    description,
    alternates: {
      canonical,
    },
    openGraph: {
      type: 'article',
      title: article.atJmeno,
      description,
      url: canonical,
      publishedTime: article.createdAt ?? undefined,
      modifiedTime: article.updatedAt ?? undefined,
      images: article.titlePhotoUrl
        ? [{ url: article.titlePhotoUrl, alt: article.atJmeno }]
        : undefined,
    },
    robots: article.isVisible ? undefined : { index: false, follow: false },
  };
}

export default async function ArticlePage({ params }: ArticlePageProps) {
  const { id, slug } = await params;
  const article = await getArticle(id);

  if (!article) notFound();

  const canonical = articlePath(article.id, article.atJmeno);
  const currentSlug = slug?.join('/') ?? '';
  const expectedSlug = slugify(article.atJmeno);

  if (currentSlug !== expectedSlug) {
    redirect(canonical);
  }

  return (
    <>
      <PageHeader title={article.atJmeno} />
      {article.createdAt && (
        <div className="text-neutral-11 mb-6 -mt-4">
          {fullDateFormatter.format(new Date(article.createdAt))}
        </div>
      )}
      <RichTextView value={article.atText} />
    </>
  );
}
