import * as React from 'react';
import { ArticleList } from '@/ui/lists/ArticleList';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@/ui/WithSidebar';

export default function ArticlesPage() {
  return (
    <Layout requireTrainer>
      <NextSeo title="Aktuality" />
      <WithSidebar sidebar={<ArticleList />} />
    </Layout>
  );
}
