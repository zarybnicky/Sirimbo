import React from 'react';
import { PaymentItemList } from '@app/ui/entity-lists';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout requireAdmin>
    <NextSeo title="Platby" />
    <WithSidebar sidebar={<PaymentItemList />} />
  </Layout>
);

export default Page;
