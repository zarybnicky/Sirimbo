import { TitleBar } from '@app/ui/TitleBar';
import { ArticlePublicList } from '@app/ui/ArticlePublicList';
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
