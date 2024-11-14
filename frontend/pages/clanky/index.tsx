import { TitleBar } from '@/ui/TitleBar';
import { ArticlePublicList } from '@/ui/lists/ArticlePublicList';
import * as React from 'react';
import { Layout } from '@/components/layout/Layout';

export default function ArticlesPage() {
  return (
    <Layout showTopMenu>
      <TitleBar title="Články" />
      <ArticlePublicList />
    </Layout>
  );
}
