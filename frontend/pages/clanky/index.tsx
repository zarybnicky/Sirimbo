import { PageHeader } from '@/ui/TitleBar';
import { ArticlePublicList } from '@/ui/lists/ArticlePublicList';
import * as React from 'react';
import { Layout } from '@/ui/Layout';
import { NextSeo } from 'next-seo';

export default function ArticlesPage() {
  return (
    <Layout showTopMenu>
      <NextSeo title="Články" />
      <PageHeader title="Články" />
      <ArticlePublicList />
    </Layout>
  );
}
