import { Heading } from '@app/ui/Heading';
import { ArticlePublicList } from '@app/ui/ArticlePublicList';
import { NextSeo } from 'next-seo';
import * as React from 'react';
import { Layout } from 'components/layout/Layout';

const Page = () => {
  return (
    <Layout showTopMenu>
      <Heading>Aktuálně</Heading>
      <NextSeo title="Články" />
      <ArticlePublicList />
    </Layout>
  );
}

export default Page;
