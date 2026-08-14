/* eslint-disable import-x/no-unused-modules */
import { ArticleForm } from '@/ui/forms/ArticleForm';
import type { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Nový článek',
  robots: { index: false, follow: false },
};

export default function AddArticlePage() {
  return <ArticleForm />;
}
