import * as React from 'react';
import { PaymentCategoryList } from '@app/ui/PaymentCategoryList';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout requireAdmin>
    <NextSeo title="Platby" />
    <WithSidebar sidebar={<PaymentCategoryList />} />
  </Layout>
);

export default Page;
