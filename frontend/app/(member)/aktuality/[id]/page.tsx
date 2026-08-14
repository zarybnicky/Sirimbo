/* eslint-disable import-x/no-unused-modules */
import { ArticleForm } from '@/ui/forms/ArticleForm';
import type { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Aktuality',
  robots: { index: false, follow: false },
};

export default async function ArticlePage({
  params,
}: {
  params: Promise<{ id: string }>;
}) {
  const { id } = await params;
  return <ArticleForm id={id} />;
}
