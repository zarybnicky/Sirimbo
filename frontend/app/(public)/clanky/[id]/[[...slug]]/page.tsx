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
  return executeGraphql(ArticleDocument, { id }).then(x => x.aktuality);
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

  return {
    title: item.atJmeno,
    description: item.atPreview
      .replaceAll(/<[^>]*>/g, ' ')
      .replaceAll(/\s+/g, ' ')
      .trim(),
    alternates: {
      canonical: `/clanky/${item.id}/${slugify(item.atJmeno)}`,
    },
    openGraph: {
      type: 'article',
      title: item.atJmeno,
      url: `/clanky/${item.id}/${slugify(item.atJmeno)}`,
      publishedTime: item.createdAt ?? undefined,
      modifiedTime: item.updatedAt ?? undefined,
      images: item.titlePhotoUrl
        ? [{ url: item.titlePhotoUrl, alt: item.atJmeno }]
        : undefined,
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
