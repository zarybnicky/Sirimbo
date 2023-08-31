import * as React from 'react';
import { CoupleList } from '@app/ui/CoupleList';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout>
    <NextSeo title="Páry" />
    <WithSidebar sidebar={<CoupleList />} />
  </Layout>
);

export default Page;
