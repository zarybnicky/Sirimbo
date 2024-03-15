import * as React from 'react';
import { ArticleList } from '@/ui/ArticleList';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@/ui/WithSidebar';

const Page = () => (
  <Layout requireTrainer>
    <NextSeo title="Aktuality" />
    <WithSidebar sidebar={<ArticleList />} />
  </Layout>
);

export default Page;
