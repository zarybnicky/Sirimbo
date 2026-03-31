import { PageHeader } from '@/ui/TitleBar';
import { ArticlePublicList } from '@/ui/lists/ArticlePublicList';
import * as React from 'react';
import { Layout } from '@/ui/Layout';

export default function ArticlesPage() {
  return (
    <Layout showTopMenu>
      <PageHeader title="Články" />
      <ArticlePublicList />
    </Layout>
  );
}
