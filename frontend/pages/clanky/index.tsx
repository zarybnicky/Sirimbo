import { TitleBar } from '@/ui/TitleBar';
import { ArticlePublicList } from '@/ui/ArticlePublicList';
import * as React from 'react';
import { Layout } from '@/components/layout/Layout';

const Page = () => {
  return (
    <Layout showTopMenu>
      <TitleBar title="Články" />
      <ArticlePublicList />
    </Layout>
  );
}

export default Page;
